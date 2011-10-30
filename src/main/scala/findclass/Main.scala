//
// findclass - finds classes in source files
// http://github.com/samskivert/findclass

package findclass

import java.io.File
import scala.collection.mutable.{Set => MSet}
import scala.io.Source

import Util._

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

  // returns path and index files given the specified parent directory
  def pomFile (parent :File) = new File(parent, "pom.xml")
  def pathFile (parent :File) = new File(parent, ".findclass.path")

  /** Returns Some(file) if the file exists, or None. */
  def fileToOpt (file :File) = if (file.exists) Some(file) else None

  /** Does the actual finding of a class. */
  def findClass (classname :String, refFile :File, opts :Opts) {
    // find the project root and any .findclass.path file in the project
    val (proot, pfcpath) = findRoot(refFile.getParentFile)
    debug("Finding class '" + classname + "' in " + proot)

    // locate the .findclass.path in our home directory, if one exists
    val home = new File(System.getProperty("user.home"))
    val hfcpath = fileToOpt(pathFile(home))

    // consolidate our zero, one or two fcpath files into a list of indexes
    val indexes = List(pfcpath, hfcpath).flatten.distinct.map(new Index(_))

    // if an index rebuild was requested, do so now
    if (opts.doRebuild) {
      indexes foreach(_.rebuild)
    }

    // first try searching the project directly; if that fails, try searching the indices
    searchProject(classname)(proot) orElse indexes.mapFirst(_.search(classname)) match {
      case None => println("nomatch")
      case Some(m) => {
        if (opts.doImport) println(Import.computeInfo(classname, refFile, m))
        else println("match " + m.file.getPath + " " + m.lineno)
      }
    }
  }

  /** Returns the project root directory and any .findclass.path file therein. */
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

  /** Searches a project's source hierarchy directly (no cache files). */
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

  /** Searches a single file for the specified named class. */
  def searchFile (classname :String)(file :File) :Option[Match] = {
    try {
      Parser.parse(file).types collectFirst {
        case (name, fqName, lineno) if (name.toLowerCase == classname) =>
          Match(file, fqName, lineno)
      }
    } catch {
      case e => warning("Failed to parse " + file + ": " + e); None
    }
  }
}
