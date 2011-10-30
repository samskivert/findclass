//
// findclass - finds classes in source files
// http://github.com/samskivert/findclass

package findclass

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source

import Util._

/**
 * Provides a mapping from classname to its original source for a collection of dependencies.
 */
class Index (fcpath :File)
{
  /** Rebuilds the cached index file for this index. */
  def rebuild () {
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

  /**
   * Searches for the specified classname in the index file corresponding to the supplied fcpath.
   * If the index doesn't exist, it is built.
   */
  def search (classname :String) :Option[Match] = {
    debug("Checking index: " + fcpath)

    val fcindex = indexFile(fcpath.getParentFile)
    if (!fcindex.exists) rebuild()

    Source.fromFile(fcindex).getLines map(_.split(":")) collectFirst {
      case Array(path, clazz, fqName, lineno) if (clazz == classname) =>
        Match(new File(path), fqName, lineno.toInt)
    }
  }

  /** Returns the src/main or src subdir of the supplied dir if it exists, or the dir. */
  private def findSrcDir (pdir :File) = {
    val srcDir = new File(pdir, "src")
    List(new File(srcDir, "main"), srcDir, pdir) find(_.isDirectory) get
  }

  /** Scans a source file tree and writes all classes to the supplied writer. */
  private def scanAndIndex (out :BufferedWriter)(pdir :File) {
    debug("Rebuilding index: " + pdir)

    val (dirs, files) = pdir.listFiles partition(_.isDirectory)

    files filter(isSource) foreach { f =>
      val fpath = f.getCanonicalPath
      try {
        Parser.parse(f).types foreach { case (name, fqName, lineno) =>
          out.write(fpath + ":" + name.toLowerCase + ":" + fqName + ":" + lineno)
          out.newLine()
        }
      } catch {
        case e => warning("Failed to parse " + fpath + ": " +e)
      }
    }

    dirs filterNot(isSkipDir) foreach scanAndIndex(out)
  }

  private def indexFile (parent :File) = new File(parent, ".findclass.cache")
}
