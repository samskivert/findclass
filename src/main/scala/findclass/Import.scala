//
// findclass - finds classes in source files
// http://github.com/samskivert/findclass

package findclass

import java.io.{File, FileReader, StreamTokenizer}
import scala.collection.mutable.{ArrayBuffer}

import Util._

/**
 * Handles the parsing of a source file's imports to determine on which line a given import should
 * be inserted.
 */
object Import
{
  /**
   * Extracts the fully qualified name for an import and then determines exactly where and how it
   * should be inserted into the reference file. Yields output of the form:
   * `match fqClassName lineNo [<nil>|blank|postblank]`
   */
  def computeInfo (classname :String, refFile :File, m :Match) = {
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

  private val PostPackageRe = "\\.[A-Z].*".r

  /** Models an import statement, handles custom collation. */
  private[findclass] case class Import (static :Boolean, fqName :String) {
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

  /** Tracks a block of imports surrounded by blank lines. */
  private case class ImportGroup (lineno :Int) {
    val imports = ArrayBuffer[Import]()
    lazy val prefix = imports map(_.toPackage) reduceLeft(sharedPrefix)
    def matchLength (imp :Import) = sharedPrefix(prefix, imp.fqName).length
    def linenoAfter = lineno + imports.size
    def linenoFor (imp :Import) = lineno + imports.count(_.compareTo(imp) < 0)
  }

  /**
   * Determines the shared package component prefix (i.e. com.foo.bar and com.foo.baz share a
   * com.foo prefix (note, not a com.foo.ba prefix); also we ignore com as a shared prefix, so
   * com.foo and com.bar will report no shared prefix.
   */
  private[findclass] def sharedPrefix (pkgA :String, pkgB :String) = {
    val (compsA, compsB) = (sansCom(pkgA.split("\\.")), sansCom(pkgB.split("\\.")))
    compsA zip(compsB) takeWhile(t => t._1 == t._2) map(_._1) mkString(".")
  }
  private def sansCom (comps :Seq[String]) = if (comps.head == "com") comps.tail else comps
}
