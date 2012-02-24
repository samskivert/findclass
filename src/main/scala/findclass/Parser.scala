//
// findclass - finds classes in source files
// http://github.com/samskivert/findclass

package findclass

import java.io.{File, FileReader, Reader, StreamTokenizer}
import scala.collection.mutable.{ArrayBuffer, Stack => MStack}

import Util._

/** Performs a very primitive form of source file parsing. */
object Parser
{
  /** Represents a component of a compilation unit; could be a package or type. */
  case class Component (name :String, parent :Component, isType :Boolean, lineno :Int) {
    val members = ArrayBuffer[Component]()
    def fqName :String = parent.mkFqName(name)
    def dump (indent :String = "") {
      println(indent + name + ":" + lineno)
      members foreach { _.dump(indent + "  ") }
    }
    def types :Seq[(String, String,Int)] =
      (if (isType) Seq(Tuple3(name, fqName, lineno)) else Seq()) ++ members.flatMap(_.types)
    protected def mkFqName (child :String) :String = parent.mkFqName(name + "." + child)
  }

  class RootComponent extends Component("<root>", null, false, -1) {
    override def mkFqName (child :String) = child
  }

  /** Parses the supplied source file. */
  def parse (file :File) :Component =
    parse(new FileReader(file), suffix(file.getName))

  /** Parses the source in `reader` which hails from a source file with suffix `suff`. */
  def parse (reader :Reader, suff :String) :Component = {
    val kinds = kindsBySuff.getOrElse(suff, Set[String]())
    val stack = MStack[Component]()
    val root = new RootComponent
    var accum :Component = root
    var last :Component = null
    var prevtok :String = null
    var skipped = 0

    val tok = toker(reader)
    // treat # as a line comment starter in C# so that we ignore compiler directives
    if (suff == ".cs") tok.commentChar('#')

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
        if (prevtok == "package" || prevtok == "namespace") {
          last = Component(tok.sval, accum, false, tok.lineno)
          accum.members += last
          // if the next token is a semicolon (or if this is Scala and the next token is not an
          // open bracket), pretend the rest of the file is one big block
          val ntok = tok.nextToken()
          if (ntok == ';' || (ntok != '{' && suff == ".scala")) {
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
