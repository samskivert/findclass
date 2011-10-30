//
// findclass - finds classes in source files
// http://github.com/samskivert/findclass

package findclass

import java.io.File
import scala.collection.mutable.{ArrayBuffer, Set => MSet}
import scala.io.Source

import Util._

/**
 * Models a project, which contains source code and has dependencies (which take the form of
 * searchable indices).
 */
class Project (
  /** This project's root directory. */
  val root :File,
  /** This projects dependencies as searchable indices. */
  val depends :Seq[Index]
) {
  /** Searches this project's source hierarchy for the specified class. */
  def search (classname :String) :Option[Match] = {
    debug("Searching project: " + root)

    val rootPath = root.getPath
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

    search(root)
  }

  /** Searches a single file for the specified named class. */
  private def searchFile (classname :String)(file :File) :Option[Match] = {
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

object Project
{
  /**
   * Constructs a project that contains the specified reference file.
   */
  def forFile (refFile :File) = {
    val refDir = refFile.getParentFile
    var indexes = ArrayBuffer[Index]()
    var root, cdir = refDir
    while (cdir != null && cdir != homeDir && root == refDir) {
      // if there's a config file in this directory, add its indices to our list
      indexes ++= pathFile(cdir).toSeq flatMap(Index.fromConfig)
      // if we haven't already identified a project root and this directory either a) looks like a
      // root, or b) contains a .findclass.path file, call it the root
      if (root == refDir && isProjectRoot(cdir)) root = cdir
      // now pop up a directory and loop
      cdir = cdir.getParentFile
    }
    new Project(root, indexes.toSeq)
  }

  /** Looks for a build file or src directory to indicate that we're in the project root. */
  private def isProjectRoot (dir :File) =
    List("src", "build.xml", "pom.xml", "build.sbt", "Makefile") exists(
      n => new File(dir, n).exists)
}
