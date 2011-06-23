//
// findclass - finds classes in source files
// http://github.com/samskivert/findclass

package findclass

object Util {
  // a simple logging mechanism
  var showDebug = false
  def debug (msg :String) :Unit = if (showDebug) System.err.println("DEBUG: " + msg)
  def warning (msg :String) :Unit = System.err.println("WARN: " + msg)

  // applies the mapping function to the supplied seq; returns first none-None, or None
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
