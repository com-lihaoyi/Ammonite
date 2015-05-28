package ammonite.terminal

import java.io.{Writer, FileOutputStream, OutputStream}

import scala.annotation.tailrec
import scala.collection.mutable


object Debug{
  val debugOutput= new FileOutputStream(new java.io.File("log"))
  def apply(s: Any) = {
    debugOutput.write((System.currentTimeMillis() + "\t\t" + s + "\n").getBytes)
  }
}
class Ansi(output: Writer){
  def save() = output.write(s"\033[s")
  def restore() = output.write(s"\033[u")
  def control(n: Int, c: Char) = output.write(s"\033[" + n + c)

  /**
   * Moves to the desired row and column, using individual
   * cursor movements. `0` is in the top/left counting up
   * towards the bottom/right, and `-1` is in the bottom/right
   * counting down towards the top/left
   */
  def moveTo(row: Int, col: Int) = {
    if (row >= 0) {
      up(9999)
      down(row)
    }else{
      down(9999)
      up(-1-row)
    }
    if (col >= 0) {
      left(9999)
      right(col)
    }else{
      right(9999)
      left(-1-col)
    }
  }
  /**
   * Move up `n` squares
   */
  def up(n: Int) = if (n == 0) "" else control(n, 'A')
  /**
   * Move down `n` squares
   */
  def down(n: Int) = if (n == 0) "" else control(n, 'B')
  /**
   * Move right `n` squares
   */
  def right(n: Int) = if (n == 0) "" else control(n, 'C')
  /**
   * Move left `n` squares
   */
  def left(n: Int) = if (n == 0) "" else control(n, 'D')

  /**
   * Clear the screen
   *
   * n=0: clear from cursor to end of screen
   * n=1: clear from cursor to start of screen
   * n=2: clear entire screen
   */
  def   clearScreen(n: Int) = control(n, 'J')
  /**
   * Clear the current line
   *
   * n=0: clear from cursor to end of line
   * n=1: clear from cursor to start of line
   * n=2: clear entire line
   */
  def clearLine(n: Int) = control(n, 'K')
}
object TTY{
  def init() = {
    val raw = stty("-a")

    val width = "(\\d+) columns;".r.findFirstMatchIn(raw).get.group(1).toInt
    val height = "(\\d+) rows;".r.findFirstMatchIn(raw).get.group(1).toInt
    Debug("Initializing, Width " + width)
    val initialConfig = stty("-g").trim
    stty("-icanon min 1 -icrnl -inlcr -ixon")
    stty("dsusp undef")
    stty("-echo")
    stty("intr undef")
    (width, height, initialConfig)
  }
  def stty(s: String) = {
    import sys.process._
    Seq("bash", "-c", s"stty $s < /dev/tty").!!
  }
  def restore(initialConfig: String) = {
    stty(initialConfig)
  }
}

/**
 * A truly-lazy implementation of scala.Stream
 */
case class LazyList[T](headThunk: () => T, tailThunk: () => LazyList[T]){
  var rendered = false
  lazy val head = {
    rendered = true
    headThunk()
  }

  lazy val tail = tailThunk()

  def dropPrefix(prefix: Seq[T]) = {
    @tailrec def rec(n: Int, l: LazyList[T]): Option[LazyList[T]] = {
      if (n >= prefix.length) Some(l)
      else if (prefix(n) == l.head) rec(n + 1, l.tail)
      else None
    }
    rec(0, this)
  }
  override def toString = {

    @tailrec def rec(l: LazyList[T], res: List[T]): List[T] = {
      if (l.rendered) rec(l.tailThunk(), l.head :: res)
      else res
    }
    s"LazyList(${(rec(this, Nil).reverse ++ Seq("...")).mkString(",")})"
  }
  def ~:(other: => T) = LazyList(() => other, () => this)
}
object LazyList{
  object ~:{
    def unapply[T](x: LazyList[T]) = Some((x.head, x.tail))
  }
  def continually[T](t: => T): LazyList[T] = LazyList(() => t, () =>continually(t))

  implicit class CS(ctx: StringContext){
    val base = ctx.parts.mkString
    object pref{
      def unapply(s: LazyList[Int]): Option[LazyList[Int]] = {
        s.dropPrefix(base.map(_.toInt))
      }
    }
  }
}