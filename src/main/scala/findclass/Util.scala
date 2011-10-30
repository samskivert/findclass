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
  def expandTwiddle (path :String) = path.replace("~", homeDir)
  private val homeDir = System.getProperty("user.home")

  /** Tokens that will appear prior to a type declaration by language file suffix. */
  val kindsBySuff = Map(".java"  -> Set("class", "enum", "interface", "@interface"),
                        ".scala" -> Set("class", "object", "trait"),
                        ".as"    -> Set("class", "interface"))

  /** Returns true if the supplied file is a source file that we grok. */
  def isSource (file :File) = kindsBySuff.contains(suffix(file.getName))

  /** Identifies directories to skip when searching. */
  def isSkipDir (dir :File) = skipDirNames(dir.getName)
  private val skipDirNames = Set(".", "..", "CVS", ".svn", ".git", ".hg")

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

  // applies the mapping function to the supplied seq; returns first non-None, or None
  class RichIterable[A] (seq :Iterable[A]) {
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
  implicit def enrichArray[A] (seq :Array[A]) = new RichIterable(seq)
}
