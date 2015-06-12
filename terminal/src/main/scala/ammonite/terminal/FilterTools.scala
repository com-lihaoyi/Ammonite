package ammonite.terminal
import acyclic.file
/**
 * Created by haoyi on 6/11/15.
 */
object FilterTools {

  /**
   * Lets you easily pattern match on characters modified by ctrl
   */
  object Ctrl{
    def apply(c: Char) = (c - 96).toString
    def unapply(i: Int): Option[Int] = Some(i + 96)
  }
  def Filters[K, V](pfs: PartialFunction[K, V]*) = new PartialFunction[K, V]{
    def isDefinedAt(x: K) = pfs.exists(_.isDefinedAt(x))

    def apply(v1: K) = pfs.find(_.isDefinedAt(v1)).map(_(v1)).getOrElse(throw new MatchError(v1))
  }
  def Alt = "\u001b"
  def Case(s: String)(f: (Vector[Char], Int, TermInfo) => (Vector[Char], Int)) = new PartialFunction[TermInfo, TermAction] {
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
  def TSX(rest: LazyList[Int], args: (Vector[Char], Int)) = {
    TS(rest, args._1, args._2)
  }

  val TS = TermState
  val TI = TermInfo


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
