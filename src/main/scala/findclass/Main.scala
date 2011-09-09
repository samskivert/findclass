//
// findclass - finds classes in source files
// http://github.com/samskivert/findclass

package findclass

import java.io.{File, Reader, StreamTokenizer}
import java.io.{BufferedReader, FileReader, BufferedWriter, FileWriter}

import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, Set => MSet, Stack => MStack}

import findclass.Util._

/**
 * The basis for a poor man's Java/Scala/ActionScript IDE, in the style of ctags, but hand rolled
 * and with extra MDB trimmings.
 */
object Main
{
  val usage = "Usage: findclass [-debug] [-import] [-rebuild] classname reference_file";

  def main (args :Array[String]) {
    val (flags, rargs) = args.partition(_ startsWith "-")
    val opts = parseOpts(flags.toSet)
    rargs match {
      // lowercase our classname to simplify string compares later
      case Array(cname, rfile) =>
        findClass(cname.toLowerCase, new File(expandTwiddle(rfile)).getCanonicalFile, opts)
      case _ => println(usage)
    }
  }

  // a case class encapsulating our options and a constructor function
  case class Opts (doImport :Boolean, doRebuild :Boolean)
  def parseOpts (flags :Set[String]) = {
    Util.showDebug = flags("-debug")
    Opts(flags("-import"), flags("-rebuild"))
  }

  // represents a class name matched in a file
  case class Match (file :File, fqName :String, lineno :Int)

  // tokens that will appear prior to a type declaration by language file suffix
  val kindsBySuff = Map(".java"  -> Set("class", "enum", "interface", "@interface"),
                        ".scala" -> Set("class", "object", "trait"),
                        ".as"    -> Set("class", "interface"))

  // returns true if the supplied file is a source file that we grok
  def isSource (file :File) = kindsBySuff.contains(suffix(file.getName))

  // directories to skip when searching
  val skipDirNames = Set(".", "..", "CVS", ".svn", ".git", ".hg")
  def isSkipDir (dir :File) = skipDirNames(dir.getName)

  // extracts the suffix from a filename (.java for Foo.java)
  def suffix (name :String) = name.lastIndexOf(".") match {
    case -1 => ""
    case idx => name.substring(idx)
  }

  // returns path and index files given the specified parent directory
  def pathFile (parent :File) = new File(parent, ".findclass.path")
  def indexFile (parent :File) = new File(parent, ".findclass.cache")

  // returns Some(file) if the file exists, or None
  def fileToOpt (file :File) = if (file.exists) Some(file) else None

  // allow ~ in paths like a good unix utility
  val homeDir = System.getProperty("user.home")
  def expandTwiddle (path :String) = path.replace("~", homeDir)

  // creates a sanely configured stream tokenizer
  def toker (reader :Reader) = {
    val tok = new StreamTokenizer(new BufferedReader(reader))
    tok.ordinaryChar('/') // wtf do they call this a comment char by default?
    tok.slashSlashComments(true)
    tok.slashStarComments(true)
    tok
  }

  // does the actual finding of a class
  def findClass (classname :String, refFile :File, opts :Opts) {
    // find the project root and any .findclass.path file in the project
    val (proot, pfcpath) = findRoot(refFile.getParentFile)
    debug("Finding class '" + classname + "' in " + proot)

    // locate the .findclass.path in our home directory, if one exists
    val home = new File(System.getProperty("user.home"))
    val hfcpath = fileToOpt(pathFile(home))

    // consolidate our zero, one or two fcpath files into a list
    val fcpaths = List(pfcpath, hfcpath).flatten.distinct

    // if an index rebuild was requested, do so now
    if (opts.doRebuild) {
      fcpaths foreach rebuildIndex
    }

    // first try searching the project directly; if that fails, try searching the indices
    searchProject(classname)(proot) orElse fcpaths.mapFirst(checkIndex(classname)) match {
      case None => println("nomatch")
      case Some(m) => {
        if (opts.doImport) println(computeImportInfo(classname, refFile, m))
        else println("match " + m.file.getPath + " " + m.lineno)
      }
    }
  }

  // returns the project root directory and any .findclass.path file therein
  def findRoot (sdir :File) :(File, Option[File]) = {
    // looks for a build file or src directory to indicate that we're in the project root
    def isProjectRoot (dir :File) =
      List("src", "build.xml", "pom.xml", "build.sbt", "Makefile") exists(
        n => new File(dir, n).exists)

    var root = sdir
    var fcpath = None :Option[File]
    var cdir = sdir
    while (cdir != null && !fcpath.isDefined) {
      val cfcpath = pathFile(cdir)
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

  // rebuilds the .findclass.index file for the supplied .findclass.path file
  def rebuildIndex (fcpath :File) {
    val homeDir = System.getProperty("user.home")
    val paths = Source.fromFile(fcpath).getLines.map(expandTwiddle)

    val fcindex = indexFile(fcpath.getParentFile)
    val out = new BufferedWriter(new FileWriter(fcindex))
    try {
      val (good, bad) = paths map(new File(_)) partition(_.exists)
      bad foreach { d => warning(fcpath + " contains non-existent " + d) }
      good map(findSrcDir) foreach scanAndIndex(out)
    } finally {
      out.close
    }
  }

  // returns the src/main or src subdir of the supplied dir if it exists, or the dir
  def findSrcDir (pdir :File) = {
    val srcDir = new File(pdir, "src")
    List(new File(srcDir, "main"), srcDir, pdir) find(_.isDirectory) get
  }

  // scans a source file tree and writes all classes to the supplied writer
  def scanAndIndex (out :BufferedWriter)(pdir :File) {
    debug("Rebuilding index: " + pdir)

    val (dirs, files) = pdir.listFiles partition(_.isDirectory)

    files filter(isSource) foreach { f =>
      val fpath = f.getCanonicalPath
      try {
        parse(f).types foreach { case (name, fqName, lineno) =>
          out.write(fpath + ":" + name.toLowerCase + ":" + fqName + ":" + lineno)
          out.newLine()
        }
      } catch {
        case e => warning("Failed to parse " + fpath + ": " +e)
      }
    }

    dirs filterNot(isSkipDir) foreach scanAndIndex(out)
  }

  // searches for the specified classname in the index file corresponding to the supplied fcpath;
  // if the index doesn't exist, it is built
  def checkIndex (classname :String)(fcpath :File) :Option[Match] = {
    debug("Checking index: " + fcpath)

    val fcindex = indexFile(fcpath.getParentFile)
    if (!fcindex.exists) {
      rebuildIndex(fcpath)
    }

    Source.fromFile(fcindex).getLines map(_.split(":")) collectFirst {
      case Array(path, clazz, fqName, lineno) if (clazz == classname) =>
        Match(new File(path), fqName, lineno.toInt)
    }
  }

  // searches a project's source hierarchy directly (no cache files)
  def searchProject (classname :String)(rootDir :File) :Option[Match] = {
    debug("Searching project: " + rootDir)

    val rootPath = rootDir.getPath
    def isSubDir (dir :File) = dir.getPath.startsWith(rootPath)

    val searched = MSet[File]()
    def search (dir :File) :Option[Match] = {
      searched += dir // avoid double checking in the face of symlinks
      val (dirs, files) = dir.listFiles partition(_.isDirectory)

      // see if any files in this directory define our class
      val searchFiles = files filter(isSource)
      // searchFiles.view flatMap(searchFile(classname)) collectFirst { case x => x } orElse {
      searchFiles mapFirst(searchFile(classname)) orElse {
        // recurse down our subdirectories in search of the class
        val searchDirs = dirs filterNot(isSkipDir) filterNot(searched) map(
          _.getCanonicalFile) filter(isSubDir) // we have to call isSubDir after getCanonicalFile
        searchDirs mapFirst(search)
      }
    }

    search(rootDir)
  }

  // searches a single file for the specified named class
  def searchFile (classname :String)(file :File) :Option[Match] = {
    try {
      parse(file).types collectFirst {
        case (name, fqName, lineno) if (name.toLowerCase == classname) =>
          Match(file, fqName, lineno)
      }
    } catch {
      case e => warning("Failed to parse " + file + ": " + e); None
    }
  }

  // determines the shared package component prefix (i.e. com.foo.bar and com.foo.baz share a
  // com.foo prefix (note, not a com.foo.ba prefix); also we ignore com as a shared prefix, so
  // com.foo and com.bar will report no shared prefix
  def sharedPrefix (pkgA :String, pkgB :String) = {
    val (compsA, compsB) = (sansCom(pkgA.split("\\.")), sansCom(pkgB.split("\\.")))
    compsA zip(compsB) takeWhile(t => t._1 == t._2) map(_._1) mkString(".")
  }
  def sansCom (comps :Seq[String]) = if (comps.head == "com") comps.tail else comps

  // models an import statement, handles custom collation
  case class Import (static :Boolean, fqName :String) {
    lazy val toPackage = PostPackageRe.replaceFirstIn(fqName, "")
    def compareTo (that :Import) = {
      val cv = collate(fqName).compareTo(collate(that.fqName)) // java, javax, everything else
      if (cv != 0) cv else {
        val bv = that.static.compareTo(this.static) // statics after non-statics
        if (bv != 0) bv else fqName.compareTo(that.fqName) // then alphabetical
      }
    }
    private def collate (fqName :String) = if (fqName.startsWith("java.")) -2
                                           else if (fqName.startsWith("javax.")) -1
                                           else 0
  }
  val PostPackageRe = "\\.[A-Z].*".r

  // tracks a block of imports surrounded by blank lines
  case class ImportGroup (lineno :Int) {
    val imports = ArrayBuffer[Import]()
    lazy val prefix = imports map(_.toPackage) reduceLeft(sharedPrefix)
    def matchLength (imp :Import) = sharedPrefix(prefix, imp.fqName).length
    def linenoAfter = lineno + imports.size
    def linenoFor (imp :Import) = lineno + imports.count(_.compareTo(imp) < 0)
  }

  // extracts the fully qualified name for an import and then determines exactly where and how it
  // should be inserted into the reference file; will yield output of the form:
  // match fqClassName lineNo [<nil>|blank|postblank]
  def computeImportInfo (classname :String, refFile :File, m :Match) = {
    var pkgLine = -1
    val igroups = ArrayBuffer[ImportGroup]()
    var accumg :ImportGroup = null
    var sawClass = false

    // parse the input groups in the file
    val kinds = kindsBySuff.getOrElse(suffix(refFile.getName), Set[String]())
    val tok = toker(new FileReader(refFile))
    tok.eolIsSignificant(true)
    while (!sawClass && tok.nextToken() != StreamTokenizer.TT_EOF) {
      if (tok.ttype == StreamTokenizer.TT_WORD) {
        // note the last time we see package appear in the file
        if (tok.sval == "package") pkgLine = tok.lineno

        if (tok.sval == "import") {
          if (accumg == null) {
            accumg = ImportGroup(tok.lineno)
            igroups += accumg
          }
          tok.nextToken() // the next token should either be 'static' or an fqName
          var static = false
          if (tok.sval == "static") { // note 'static' Java static imports
            static = true
            tok.nextToken()
          }
          accumg.imports += Import(static, tok.sval)
        }

        // if we see a class, etc. declaration, stop parsing import groups
        sawClass = kinds(tok.sval)

      } else if (tok.ttype == StreamTokenizer.TT_EOL) {
        // if we see a blank line (EOL + EOL), clear out any accumulating import group
        if (tok.nextToken() == StreamTokenizer.TT_EOL) accumg = null
        else tok.pushBack()
      }
    }

    val newimp = Import(false, m.fqName)
    if (igroups.isEmpty) {
      // if we have no import groups, insert this import after the package statement
      "match " + m.fqName + " " + (pkgLine+1) + " preblank"
    } else {
      val best = igroups maxBy(_.matchLength(newimp))
      if (best.imports contains(newimp)) "notneeded"
      // the best prefix may be degenerate (i.e. all of the groups match with zero length, so the
      // best is an arbitrary choice), in this case we want to create a new import group *unless*
      // the best group itself has unrelated packages in it (i.e. its internal shared prefix length
      // is zero); this probably means the file jams all imports together in one big group
      else if (best.matchLength(newimp) == 0 && best.prefix.length > 0) {
        // no matching group, so figure out where to insert based on collation between groups
        igroups find(_.imports.head.compareTo(newimp) > 0) match {
          // if we collate before a particular import group, place our import before that group's
          // first import, followed by a blank line
          case Some(ig) => "match " + m.fqName + " " + ig.lineno + " postblank"
          // if we don't collate before any of the import groups, place our import after the last
          // import group, with a preceding blank line
          case None => "match " + m.fqName + " " + igroups.last.linenoAfter + " preblank"
        }
      } else {
        // determine where in our matched import group we should be inserted (based on alphabetic
        // ordering, with "import static" elements always coming last)
        "match " + m.fqName + " " + best.linenoFor(newimp) + " noblank"
      }
    }
  }

  // represents a component of a compilation unit; could be a package or type
  case class Component (name :String, parent :Component, isType :Boolean, lineno :Int) {
    val members = ArrayBuffer[Component]()
    def fqName :String = parent.mkFqName(name)
    def dump (indent :String = "") {
      println(indent + name)
      members foreach { _.dump(indent + "  ") }
    }
    def types :Seq[(String, String,Int)] =
      (if (isType) Seq(Tuple3(name, fqName, lineno)) else Seq()) ++ members.flatMap(_.types)
    protected def mkFqName (child :String) :String = parent.mkFqName(name + "." + child)
  }
  class RootComponent extends Component("<root>", null, false, -1) {
    override def mkFqName (child :String) = child
  }

  // performs a very primitive form of source file parsing
  def parse (file :File) :Component =
    parse(new FileReader(file), suffix(file.getName))

  def parse (reader :Reader, suff :String) :Component = {
    val kinds = kindsBySuff.getOrElse(suff, Set[String]())
    val tok = toker(reader)
    val stack = MStack[Component]()
    val root = new RootComponent
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
}
