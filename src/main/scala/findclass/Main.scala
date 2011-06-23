//
// $Id$

package findclass

import java.io.File

import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, Set => MSet}

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
  case class Match (file :File, lineno :Int)

  // does the actual finding of a class
  def findClass (classname :String, refFile :File, opts :Opts) {
    // find the project root and any .findclass.path file in the project
    val (proot, pfcpath) = findRoot(refFile.getParentFile)

    // locate the .findclass.path in our home directory, if one exists
    val home = new File(System.getProperty("user.home"))
    val hfcpath = fileToOpt(new File(home, ".findclass.path"))

    // consolidate our zero, one or two fcpath files into a list
    val fcpaths = List(pfcpath, hfcpath).flatten

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
        if (opts.doImport) println()
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
      val Array(path, clazz, lineno) = l.split(":")
      if (clazz == classname) {
        return Some(Match(new File(path), lineno.toInt))
      }
    }
    None
  }

  // a regular expression that matches a package declaration
  val PackageRe = "package\\s+(\\S+)".r

  // a regular expression that matches a class declaration
  val ClassRe = "class\\s+(\\S+)".r

  // an iterator that filters block comments from an underlying string iterator;
  // doesn't handle nested block comments
  class CommentFilterer (iter :Iterator[String]) extends Iterator[String] {
    var _next :String = null
    var _inComment = false
    skipComments()

    override def hasNext = (_next != null)

    override def next () = {
      if (!hasNext) throw new NoSuchElementException
      val n = _next
      skipComments()
      n
    }

    private def skipComments () {
      _next = null
      while (_next == null && iter.hasNext) {
        val n = iter.next
        if (_inComment) {
          val cend = n.indexOf("*/")
          if (n != -1) {
            _next = n.substring(cend+2)
          }
        } else {
          val cstart = n.indexOf("/*");
          if (cstart != -1) {
            _next = n.substring(0, cstart);
            _inComment = true
          } else {
            _next = n
          }
        }
      }
    }
  }

  def extractFQName (file :File, classname :String) :Option[String] = {
    var pkg = null
    var cname = null
    val lines = Source.fromFile(file)
    while (lines.hasNext) {
    }
    None
  }

  // directories to skip when searching
  val skipDirs = Set(".", "..", "CVS", ".subversion", ".git", ".hg")

  // suffixes that identiy source files
  val sourceSuffs = Set(".java", ".groovy", ".scala", ".cs", ".as")

  // extracts the suffix from a filename (.java for Foo.java)
  def suffix (name :String) = name.lastIndexOf(".") match {
    case -1 => ""
    case idx => name.substring(idx)
  }

  def defines (file :File, classname :String) :Option[Match] = {
    println("searching " + file)
    var lineno = 0
    for (l <- Source.fromFile(file)) {
      lineno += 1
    }
    None
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
