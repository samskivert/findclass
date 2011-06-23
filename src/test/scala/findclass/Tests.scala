//
// $Id$

package findclass

import org.junit.Assert._
import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

import findclass.Main._

class Tests extends AssertionsForJUnit
{
  @Test def checkOneLineComment {
    val iter = mkFiltIter("Blah blah.\n" +
                          "/* Look ma, a block comment. */\n" +
                          "More blah blah.")
    println(iter.mkString("\n"))
  }

  def mkFiltIter (text :String) =
    new CommentFilterer(text.split("\n").iterator)
}
