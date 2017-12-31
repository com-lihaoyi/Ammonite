package ammonite.terminal

import java.io.{OutputStream, ByteArrayOutputStream, Writer}

import scala.annotation.tailrec

/**
  * Prints stuff to an ad-hoc logging file when running the ammonite repl or
  * ammonite-terminal in development mode in its SBT project.
  *
  * Very handy for the common case where you're debugging terminal interactions
  * and cannot use `println` because it will stomp all over your already messed
  * up terminal state and block debugging. With [[Debug]], you can have a
  * separate terminal open tailing the log file and log as verbosely as you
  * want without affecting the primary terminal you're using to interact with
  * Ammonite.
  */
object Debug{
  lazy val debugOutput = {
    if (System.getProperty("ammonite-sbt-build") != "true") ???
    else new java.io.FileOutputStream(new java.io.File("terminal/target/log"))
  }
  def apply(s: Any) = {
    if (System.getProperty("ammonite-sbt-build") == "true")
      debugOutput.write((System.currentTimeMillis() + "\t\t" + s + "\n").getBytes)
  }
}

/**
  * A small helper class to quickly define enumerations: extend this class in
  * your companion object and define the items via `val a, b, c = Item(...)`
  * passing in your enum class constructor in place of `...`.
  */
abstract class Enum(implicit enumName: sourcecode.Name){
  protected[this] def Item[T](constructor: String => T)
                             (implicit i: sourcecode.Name): T = {
    constructor(enumName.value + "." + i.value)
  }
}

class AnsiNav(output: Writer){
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
object AnsiNav{
  val resetUnderline = "\u001b[24m"
  val resetForegroundColor = "\u001b[39m"
  val resetBackgroundColor = "\u001b[49m"
}

object TTY{

  // Prefer standard tools. Not sure why we need to do this, but for some
  // reason the version installed by gnu-coreutils blows up sometimes giving
  // "unable to perform all requested operations"
  val pathedStty: String = if (new java.io.File("/bin/stty").exists()) "/bin/stty" else "stty"
  def consoleDims: (Int, Int) = stty("size").trim.split(" ") match {
    case Array(height, width) => (height.toInt, width.toInt)
  }

  def consoleWidth: Int = consoleDims._2
  def consoleHeight: Int = consoleDims._1

  def init() = {
    stty("-a")

//    Debug("Initializing, Width " + width)
//    Debug("Initializing, Height " + height)
    val initialConfig = stty("-g").trim
    stty("-icanon min 1 -icrnl -inlcr -ixon")
    sttyFailTolerant("dsusp undef")
    stty("-echo")
    stty("intr undef")
//    Debug("")
    (consoleWidth, consoleHeight, initialConfig)
  }

  private def sttyCmd(s: String) = {
    import sys.process._
    Seq("sh", "-c", s"$pathedStty $s < /dev/tty"): ProcessBuilder
  }

  def stty(s: String): String =
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
/**
  * Implicitly instantiated class letting you pass in a single string or a
  * sequence of strings anywhere a set of prefixes is required
  */
case class Strings(values: Seq[String])
object Strings{
  implicit def stringPrefix(s: String) = Strings(Seq(s))
  implicit def stringSeqPrefix(s: Seq[String]) = Strings(s)
}
