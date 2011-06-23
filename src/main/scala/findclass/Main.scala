//
// $Id$

package findclass

import java.io.{File, BufferedReader, FileReader, Reader, StreamTokenizer}

import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, Set => MSet, Stack => MStack}

/**
 * The basis for a poor man's Java/Scala/ActionScript IDE, in the style of ctags, but hand rolled
 * and with extra MDB trimmings.
 */
object Main
{
  val usage = "Usage: findclass [-debug] [-import] [-rebuild] classname reference_file";

  // a case class encapsulating our options and a constructor function
  case class Opts (debug :Boolean, doImport :Boolean, doRebuild :Boolean)
  def parseOpts (flags :Set[String]) =
    Opts(flags("-debug"), flags("-import"), flags("-rebuild"))

  def main (args :Array[String]) {
    val (flags, rargs) = args.partition(_ startsWith "-")
    val opts = parseOpts(flags.toSet)
    rargs match {
      // lowercase our classname to simplify string compares later
      case Array(cname, rfile) =>
        findClass(cname.toLowerCase, new File(rfile).getCanonicalFile, opts)
      case _ => println(usage)
    }
  }

  // represents a class name matched in a file
  case class Match (file :File, fqName :String, lineno :Int)

  // does the actual finding of a class
  def findClass (classname :String, refFile :File, opts :Opts) {
    // find the project root and any .findclass.path file in the project
    val (proot, pfcpath) = findRoot(refFile.getParentFile)

    // locate the .findclass.path in our home directory, if one exists
    val home = new File(System.getProperty("user.home"))
    val hfcpath = fileToOpt(new File(home, ".findclass.path"))

    // consolidate our zero, one or two fcpath files into a list
    val fcpaths = List(pfcpath, hfcpath).flatten.distinct
    print("FCPaths " + fcpaths)

    // if an index rebuild was requested, do so now
    if (opts.doRebuild) {
      fcpaths foreach rebuildIndex
    }

    // create a list of actions to try successively to find the class, and run them
    val finders = Seq[String => Option[Match]]() ++
      fcpaths.map(p => checkIndex(p) _) ++
      List(searchProject(proot) _)

    finders.view flatMap(f => f(classname)) collectFirst { case x => x } match {
      case None => println("nomatch")
      case Some(m) => {
        if (opts.doImport) println(computeImportInfo(classname, refFile, m))
        else println("match " + m.file.getPath + " " + m.lineno)
      }
    }
  }

  // returns the project root directory and any .findclass.path file therein
  def findRoot (sdir :File) :(File, Option[File]) = {
    var root = sdir
    var fcpath = None :Option[File]
    var cdir = sdir
    while (cdir != null && !fcpath.isDefined) {
      val cfcpath = new File(cdir, ".findclass.path")
      if (cfcpath.exists) {
        fcpath = Some(cfcpath)
      }
      // if we haven't already identified a project root and this directory either a) looks like a
      // root, or b) contains a .findclass.path file, call it the root
      if (root == sdir && (isProjectRoot(cdir) || fcpath.isDefined)) {
        root = cdir
      }
      cdir = cdir.getParentFile
    }
    (root, fcpath)
  }

  // returns Some(file) if the file exists, or None
  def fileToOpt (file :File) = if (file.exists) Some(file) else None

  // looks for a build file or src directory to indicate that we're in the project root
  def isProjectRoot (dir :File) =
    List("src", "build.xml", "pom.xml", "build.sbt", "Makefile") exists(
      n => new File(dir, n).exists)

  // rebuilds the .findclass.index file for the supplied .findclass.path file
  def rebuildIndex (fcpath :File) {
    println("Rebuilding index in " + fcpath.getParentFile)

    val homeDir = System.getProperty("user.home")
    val paths = Source.fromFile(fcpath).getLines.map(_.replace("~", homeDir))
    println("Paths " + paths.mkString("\n"))
  }

  // searches the index file corresponding to the supplied fcpath file for the specified classname;
  // if the index doesn't exist, it is built
  def checkIndex (fcpath :File)(classname :String) :Option[Match] = {
    println("Checking index " + fcpath)

    val ifile = new File(fcpath.getParentFile, ".findclass.cache")
    if (!ifile.exists) {
      rebuildIndex(fcpath)
    }

    // oh, I'm so imperative!
    for (l <- Source.fromFile(ifile).getLines) {
      try {
        val Array(path, clazz, fqName, lineno) = l.split(":")
        if (clazz == classname) {
          return Some(Match(new File(path), fqName, lineno.toInt))
        }
      } catch {
        case e :MatchError => // skip malformed line
      }
    }
    None
  }

  // extracts the fully qualified name for an import and then determines exactly where and how it
  // should be inserted into the reference file; will yield output of the form:
  // match fqClassName lineNo [<nil>|blank|postblank]
  def computeImportInfo (classname :String, refFile :File, m :Match) = {
    // TODO: determine insertion line
    "match " + m.fqName + " " + m.lineno
  }

  // represents a component of a compilation unit; could be a package or type
  case class Component (name :String, parent :Component, isType :Boolean, lineno :Int) {
    val members = ArrayBuffer[Component]()
    def fqName :String = parent.mkFqName(name)
    def dump (indent :String = "") {
      println(indent + name)
      members foreach { _.dump(indent + "  ") }
    }
    def types :Seq[(String,Int)] =
      (if (isType) Seq(fqName -> lineno) else Seq()) ++ members.flatMap(_.types)
    protected def mkFqName (child :String) :String = parent.mkFqName(name + "." + child)
  }
  object RootComponent extends Component("<root>", null, false, -1) {
    override def mkFqName (child :String) = child
  }

  def parse (file :File) :Component = parse(new FileReader(file), suffix(file.getName))

  def parse (reader :Reader, suff :String) :Component = {
    val kinds = kindsBySuff.getOrElse(suff, Set[String]())

    val tok = new StreamTokenizer(new BufferedReader(reader))
    tok.slashSlashComments(true)
    tok.slashStarComments(true)

    val stack = MStack[Component]()
    val root = RootComponent
    var accum :Component = root
    var last :Component = null
    var prevtok :String = null
    var skipped = 0

    while (tok.nextToken() != StreamTokenizer.TT_EOF) {
      if (tok.ttype == '{') {
        if (last != null) {
          stack.push(accum)
          accum = last
          last = null
        } else {
          skipped += 1
        }
      } else if (tok.ttype == '}') {
        last = null
        if (skipped == 0) {
          accum = stack.pop()
        } else {
          skipped -= 1
        }
      } else if (tok.ttype == StreamTokenizer.TT_WORD) {
        if (prevtok == "package") {
          last = Component(tok.sval, accum, false, tok.lineno)
          accum.members += last
          // if the next token is a semicolon, pretend the rest of the file is one big block
          if (tok.nextToken() == ';') {
            stack.push(accum)
            accum = last
            last = null
          } else {
            tok.pushBack()
          }
        } else if (kinds(prevtok)) {
          last = Component(tok.sval, accum, true, tok.lineno)
          accum.members += last
        }
        prevtok = tok.sval
      }
    }

    root
  }

  // tokens taht will appear prior to a type declaration by language file suffix
  val kindsBySuff = Map(".java"  -> Set("class", "enum", "interface", "@interface"),
                        ".scala" -> Set("class", "object", "trait"),
                        ".as"    -> Set("class", "interface"))

  // directories to skip when searching
  val skipDirs = Set(".", "..", "CVS", ".subversion", ".git", ".hg")

  // suffixes that identiy source files
  val sourceSuffs = Set(".java", ".groovy", ".scala", ".cs", ".as")

  // extracts the suffix from a filename (.java for Foo.java)
  def suffix (name :String) = name.lastIndexOf(".") match {
    case -1 => ""
    case idx => name.substring(idx)
  }

  def searchProject (rootDir :File)(classname :String) :Option[Match] = {
    println("Searching project " + rootDir)
    // searched += inDir
    // val (dirs, files) = inDir.listFiles partition(_.isDirectory)

    // // see if any files in this directory define our class
    // val local = for (f <- files
    //                  if (sourceSuffs(suffix(f.getName)));
    //                  m <- defines(f, classname)) yield m

    None
  }
}
