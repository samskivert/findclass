//
// $Id$

package findclass

import java.io.StringReader

import org.junit.Assert._
import org.junit.Test

import findclass.Main._

class Tests
{
  @Test def testJavaParse {
    val code = """
      // some comments
      package foo.b;
      import foo.baz.Biffle;
      /** Some more comments. */
      class Foozle {
        static enum Barzle { FOO, BAR, BAZ }
        interface Bizzle {}
        void foo () {
        }
      }
      """;
    assertEquals(Seq(("foo.b.Foozle", 6), ("foo.b.Foozle.Barzle", 7), ("foo.b.Foozle.Bizzle", 8)),
                 parse(new StringReader(code), ".java").types)
  }

  @Test def testScalaParse {
    val code = """
      package foo {
        package bar {
          import foo.baz.Biffle;
          /** Some comments. */
          trait Foo {
            object Barzle { }
            def foo () {
              case class Bizzle (bar :Int)
              case class Bangle ()
            }
          }
        }
      }
      """;
    assertEquals(Seq(("foo.bar.Foo", 6), ("foo.bar.Foo.Barzle", 7), ("foo.bar.Foo.Bizzle", 9),
                     ("foo.bar.Foo.Bangle", 10)),
                 parse(new StringReader(code), ".scala").types)
  }
}
