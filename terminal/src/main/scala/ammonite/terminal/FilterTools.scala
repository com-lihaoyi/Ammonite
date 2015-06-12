package ammonite.terminal
import acyclic.file
/**
 * A collection of helpers that to simpify the common case of building filters
 */
object FilterTools {

  /**
   * Lets you easily pattern match on characters modified by ctrl,
   * or convert a character into its ctrl-ed version
   */
  object Ctrl{
    def apply(c: Char) = (c - 96).toString
    def unapply(i: Int): Option[Int] = Some(i + 96)
  }

  /**
   * `orElse`-s together each partial function passed to it
   */
  def orElseAll[T, V](pfs: PartialFunction[T, V]*) = new PartialFunction[T, V]{
    def isDefinedAt(x: T) = pfs.exists(_.isDefinedAt(x))
    def apply(v1: T) = pfs.find(_.isDefinedAt(v1)).map(_(v1)).getOrElse(throw new MatchError(v1))
  }

  /**
   * The string value you get when you hit the alt key
   */
  def Alt = "\u001b"

  /**
   * Shorthand to construct a filter in the common case where you're
   * switching on the prefix of the input stream and want to run some
   * transformation on the buffer/cursor
   */
  def Case(s: String)(f: (Vector[Char], Int, TermInfo) => (Vector[Char], Int)) =
    new PartialFunction[TermInfo, TermAction] {
      def isDefinedAt(x: TermInfo) = {

        def rec(i: Int, c: LazyList[Int]): Boolean = {
          if (i >= s.length) true
          else if (c.head == s(i)) rec(i+1, c.tail)
          else false
        }
        rec(0, x.ts.inputs)
      }

      def apply(v1: TermInfo) = {
        val (buffer1, cursor1) = f(v1.ts.buffer, v1.ts.cursor, v1)
        TermState(
          v1.ts.inputs.dropPrefix(s.map(_.toInt)).get,
          buffer1,
          cursor1
        )
      }
    }

  /**
   * Shorthand for pattern matching on [[TermState]]
   */
  val TS = TermState


  def findChunks(b: Vector[Char], c: Int) = {
    val chunks = TermCore.splitBuffer(b)
    // The index of the first character in each chunk
    val chunkStarts = chunks.inits.map(x => x.length + x.sum).toStream.reverse
    // Index of the current chunk that contains the cursor
    val chunkIndex = chunkStarts.indexWhere(_ > c) match {
      case -1 => chunks.length-1
      case x => x - 1
    }
    (chunks, chunkStarts, chunkIndex)
  }

}
