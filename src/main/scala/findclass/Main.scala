//
// findclass - finds classes in source files
// http://github.com/samskivert/findclass

package findclass

import java.io.File

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
    def parseOpts (fset :Set[String]) = {
      Util.showDebug = fset("-debug")
      Opts(fset("-import"), fset("-rebuild"))
    }
    val opts = parseOpts(flags.toSet)
    rargs match {
      case Array(cname, rfile) =>
        // lowercase our classname to simplify string compares later
        findClass(cname.toLowerCase, new File(expandTwiddle(rfile)).getCanonicalFile, opts)
      case _ => println(usage)
    }
  }

  /** Encapsulates our options. */
  case class Opts (doImport :Boolean, doRebuild :Boolean)

  /** Does the actual finding of a class. */
  def findClass (classname :String, refFile :File, opts :Opts) {
    // make sure our dot directory exists
    ensureDirExists(dotDir)

    // locate the .findclass.path in our home directory, if one exists
    val home = new File(System.getProperty("user.home"))
    val hfcpath = pathFile(home)

    // determine which project contains our reference file
    val project = Project.forFile(refFile)
    debug("Finding class '" + classname + "' in " + project.root)

    // obtain our indices
    val indexes = project.depends ++ hfcpath.toSeq.flatMap(Index.fromConfig)
    // if an index rebuild was requested, do so now
    if (opts.doRebuild) indexes foreach(_.rebuild)

    // first try searching the project directly; if that fails, try searching the indices
    project.search(classname) orElse indexes.mapFirst(_.search(classname)) match {
      case None => println("nomatch")
      case Some(m) => {
        if (opts.doImport) println(Import.computeInfo(classname, refFile, m))
        else println("match " + m.file.getPath + " " + m.lineno)
      }
    }
  }
}
