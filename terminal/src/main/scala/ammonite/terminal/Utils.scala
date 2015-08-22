package ammonite.terminal

import java.io.Writer
import acyclic.file
import scala.annotation.tailrec


object Debug{
//  val debugOutput= new FileOutputStream(new java.io.File("log"))
  def apply(s: Any) = {
//    debugOutput.write((System.currentTimeMillis() + "\t\t" + s + "\n").getBytes)
  }
}
class Ansi(output: Writer){
  def control(n: Int, c: Char) = output.write(s"\033[" + n + c)

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
  def clearScreen(n: Int) = control(n, 'J')
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
  def consoleDim(s: String) = {
    import sys.process._
    Seq("bash", "-c", s"tput $s 2> /dev/tty").!!.trim.toInt
  }
  def init() = {
    val raw = stty("-a")
    import sys.process._

    val width = consoleDim("cols")
    val height = consoleDim("lines")
//    Debug("Initializing, Width " + width)
//    Debug("Initializing, Height " + height)
    val initialConfig = stty("-g").trim
    stty("-icanon min 1 -icrnl -inlcr -ixon")
    // this command fails on ubuntu, but that is harmless.
    sttyFailTolerant("dsusp undef")
    stty("-echo")
    stty("intr undef")
//    Debug("")
    (width, height, initialConfig)
  }

  private def sttyCmd(s: String) = {
    import sys.process._
    Seq("bash", "-c", s"stty $s < /dev/tty"): ProcessBuilder
  }

  def stty(s: String) =
    sttyCmd(s).!!
  /*
   * Executes a stty command for which failure is expected, hence the return
   * status can be non-null and errors are ignored.
   * This is appropriate for `stty dsusp undef`, since it's unsupported on Linux
   * (http://man7.org/linux/man-pages/man3/termios.3.html).
   */
  def sttyFailTolerant(s: String) =
    sttyCmd(s ++ " 2> /dev/null").!

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
    object p{
      def unapply(s: LazyList[Int]): Option[LazyList[Int]] = {
        s.dropPrefix(base.map(_.toInt))
      }
    }
  }
}