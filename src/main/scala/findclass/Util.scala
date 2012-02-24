//
// findclass - finds classes in source files
// http://github.com/samskivert/findclass

package findclass

import java.io.{BufferedReader, File, Reader, StreamTokenizer}

object Util {
  // a simple logging mechanism
  var showDebug = false
  def debug (msg :String) :Unit = if (showDebug) System.err.println("DEBUG: " + msg)
  def warning (msg :String) :Unit = System.err.println("WARN: " + msg)

  /** Represents a class name matched in a file. */
  case class Match (file :File, fqName :String, lineno :Int)

  /** Expands ~ in paths, like a good unix utility should. */
  def expandTwiddle (path :String) = path.replace("~", homePath)
  private val homePath = System.getProperty("user.home")

  /** The user's home directory. */
  val homeDir = new File(homePath).getCanonicalFile

  /** A directory where we store things. */
  val dotDir = new File(homeDir, ".findclass")

  /** Returns Some(file) if the file exists, or None. */
  def fileToOpt (file :File) = if (file.exists) Some(file) else None

  /** Returns an option on the findclass config file in the supplied directory. */
  def pathFile (parent :File) = fileToOpt(new File(parent, ".findclass.path"))

  /** Tokens that will appear prior to a type declaration, by language file suffix. */
  val kindsBySuff = Map(".java"  -> Set("class", "enum", "interface", "@interface"),
                        ".scala" -> Set("class", "object", "trait"),
                        ".as"    -> Set("class", "interface"),
                        ".cs"    -> Set("class", "enum", "interface", "struct"))

  /** Returns true if the supplied file is a source file that we grok. */
  def isSource (file :File) = kindsBySuff.contains(suffix(file.getName))

  /** Identifies directories to skip when searching. */
  def isSkipDir (dir :File) = skipDirNames(dir.getName)
  private val skipDirNames = Set(".", "..", "CVS", ".svn", ".git", ".hg")

  /** Ensures that the specified directory exists, creating it if needed. Issues a warning if a
   * file exists in the directory's place.
   * @return the directory, for chaining. */
  def ensureDirExists (dir :File) :File = {
    if (!dir.exists) dir.mkdir()
    else if (!dir.isDirectory) warning(dir + " must be a directory.")
    dir
  }

  /** Extracts the suffix from a filename (.java for Foo.java). */
  def suffix (name :String) = name.lastIndexOf(".") match {
    case -1 => ""
    case idx => name.substring(idx)
  }

  /** Creates a sanely configured stream tokenizer. */
  def toker (reader :Reader) = {
    val tok = new StreamTokenizer(new BufferedReader(reader))
    tok.ordinaryChar('/') // why do they call this a comment char by default?
    tok.wordChars('_', '_') // allow _ in class names
    tok.slashSlashComments(true)
    tok.slashStarComments(true)
    tok
  }

  class RichIterable[A] (seq :Iterable[A]) {
    /** Applies the mapping function to the supplied seq; returns first non-None, or None. */
    def mapFirst[B] (f :A => Option[B]) :Option[B] = {
      val iter = seq.iterator
      while (iter.hasNext) {
        val r = f(iter.next)
        if (r.isDefined) return r
      }
      None
    }
  }
  implicit def enrichIterable[A] (seq :Iterable[A]) = new RichIterable(seq)
  // we need this one as well as scalac won't go Array -> Iterable -> RichIterable
  implicit def enrichArray[A] (arr :Array[A]) = new RichIterable(arr)
}
