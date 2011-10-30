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
class Index (root :File)
{
  /** Rebuilds the cached index file for this index. */
  def rebuild () {
    debug("Rebuilding index: " + root)
    val out = new BufferedWriter(new FileWriter(cacheFile(root)))
    try scanAndIndex(out)(root)
    finally out.close
  }

  /**
   * Searches for the specified classname in the index file corresponding to the supplied fcpath.
   * If the index doesn't exist, it is built.
   */
  def search (classname :String) :Option[Match] = {
    debug("Checking index: " + root)

    val fcache = cacheFile(root)
    if (!fcache.exists) rebuild()

    Source.fromFile(fcache).getLines map(_.split(":")) collectFirst {
      case Array(path, clazz, fqName, lineno) if (clazz == classname) =>
        Match(new File(path), fqName, lineno.toInt)
    }
  }

  /** Scans a source file tree and writes all classes to the supplied writer. */
  private def scanAndIndex (out :BufferedWriter)(pdir :File) {
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

  private def cacheFile (parent :File) = {
    val cacheDir = ensureDirExists(new File(dotDir, "cache"))
    new File(cacheDir, parent.getAbsolutePath.replace(File.separatorChar, '@'))
  }
}

object Index
{
  def fromConfig (file :File) :Iterator[Index] = {
    val paths = Source.fromFile(file).getLines.map(expandTwiddle)
    val (good, bad) = paths map(new File(_)) partition(_.exists)
    bad foreach { d => warning(file + " contains non-existent " + d) }
    good map(findSrcDir) map(new Index(_))
  }

  /** Returns the src/main or src subdir of the supplied dir if it exists, or the dir. */
  private def findSrcDir (pdir :File) = {
    val srcDir = new File(pdir, "src")
    List(new File(srcDir, "main"), srcDir, pdir) find(_.isDirectory) get
  }
}
