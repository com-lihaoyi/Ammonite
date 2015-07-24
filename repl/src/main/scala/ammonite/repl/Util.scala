package ammonite.repl

import acyclic.file
import pprint.{PPrinter, PPrint}

import scala.util.Try

object Res{
  def apply[T](o: Option[T], errMsg: => String) = o match{
    case Some(s) => Success(s)
    case None => Failure(errMsg)
  }
  def apply[T](o: Try[T], errMsg: Throwable => String) = o match{
    case util.Success(s) => Success(s)
    case util.Failure(t) => Failure(errMsg(t))
  }

  /**
   * Successes map and flatmap just like a simple Box[T]
   */
  case class Success[+T](s: T) extends Res[T] {
    def flatMap[V](f: T => Res[V]): Res[V] = f(s) match {
      case Success(v) => Success(v)
      case other => other
    }

    def map[V](f: T => V): Res[V] = Success(f(s))
  }

  /**
   * Failing results never call their callbacks, and just remain unchanged
   */
  sealed abstract class Failing extends Res[Nothing]{
    def flatMap[V](f: Nothing => Res[V]): Res[V] = this
    def map[V](f: Nothing => V): Res[V] = this
  }
  case class Failure(s: String) extends Failing
  case class Exception(t: Throwable, s: String) extends Failing

  case object Skip extends Failing
  case object Exit extends Failing
}

/**
 * The result of a single pass through the ammonite REPL.
 */
sealed abstract class Res[+T]{
  def flatMap[V](f: T => Res[V]): Res[V]
  def map[V](f: T => V): Res[V]
  def filter(f: T => Boolean): Res[T] = this
}

/**
 * Exception for reporting script compilation failures
 */ 
class CompilationError(message: String) extends Exception(message)

/**
 * Fake for-comprehension generator to catch errors and turn
 * them into [[Res.Failure]]s
 */
case class Catching(handler: PartialFunction[Throwable, Res.Failing]) {

  def foreach[T](t: Unit => T): T = t(())
  def flatMap[T](t: Unit => Res[T]): Res[T] =
    try{t(())} catch handler
  def map[T](t: Unit => T): Res[T] =
    try Res.Success(t(())) catch handler
}

case class Evaluated(wrapper: String,
                     imports: Seq[ImportData])

case class ImportData(fromName: String,
                      toName: String,
                      wrapperName: String,
                      prefix: String)

/**
 * Encapsulates a read-write cell that can be passed around
 */
trait StableRef[T]{

  def apply(): T
  def update(t: T): Unit
}

trait Ref[T] extends StableRef[T]{
  def live(): () => T
  def bind(t: => T): Unit
}
object Ref{
  implicit def refer[T](t: T): Ref[T] = Ref(t)
  implicit def refPPrint[T: PPrint]: PPrinter[Ref[T]] = PPrinter{ (ref, cfg) =>
    Iterator(cfg.colors.prefixColor, "Ref", cfg.colors.endColor, "(") ++
    implicitly[PPrint[T]].pprinter.render(ref(), cfg) ++
    Iterator(")")
  }
  def live[T](value0: () => T) = new Ref[T]{
    var value: () => T = value0
    def live() = value
    def apply() = value()
    def update(t: T) = value = () => t
    def bind(t: => T): Unit = value = () => t
    override def toString = s"Ref($value)"
  }
  def apply[T](value0: T) = live(() => value0)
}
trait Cell[T]{
  def apply(): T
  def update(): Unit
}
object Cell{
  class LazyHolder[T](t: => T){
    lazy val value = t
  }
  def apply[T](t: => T) = new Cell[T] {
    var inner: LazyHolder[T] = new LazyHolder(t)
    def update() = inner = new LazyHolder(t)
    def apply() = inner.value
  }
}
/**
 * Nice pattern matching for chained exceptions
 */
object Ex{
  def unapplySeq(t: Throwable): Option[Seq[Throwable]] = {
    def rec(t: Throwable): List[Throwable] = {
      t match {
        case null => Nil
        case t => t :: rec(t.getCause)
      }
    }
    Some(rec(t))
  }
}

object Util{
  type IvyMap = Map[(String, String, String), Set[String]]
  type ClassFiles = Traversable[(String, Array[Byte])]
  type CompileCache = (ClassFiles, Seq[ImportData])
  def transpose[A](xs: List[List[A]]): List[List[A]] = xs.filter(_.nonEmpty) match {
    case Nil    =>  Nil
    case ys: List[List[A]] => ys.map{ _.head }::transpose(ys.map{ _.tail })
  }
}

object Timer{
  var current = 0L
  def reset() = current = System.nanoTime()
  /**
   * Prints the time, in millis, that has passed
   * since the last time `reset` or `apply` was called
   */
  def apply(s: String) = {
    val now = System.nanoTime()
    println(s + ": " + (now - current) / 1000000.0)
    current = now
  }
}


/**
 * A set of colors used to highlight the miscellanious bits of the REPL.
 * Re-used all over the place in PPrint, TPrint, syntax highlighting,
 * command-echoes, etc. in order to keep things consistent
 *
 * @param prompt The command prompt
 * @param ident Definition of top-level identifiers
 * @param `type` Definition of types
 * @param literal Strings, integers and other literal expressions
 * @param prefix The Seq/Foo when printing a Seq(...) or case class Foo(...)
 * @param selected The color of text selected in the line-editor
 * @param error The color used to print error messages of all kinds
 * @param reset Whatever is necessary to get rid of residual coloring
 */
case class Colors(prompt: Ref[String],
                  ident: Ref[String],
                  `type`: Ref[String],
                  literal: Ref[String],
                  prefix: Ref[String],
                  comment: Ref[String],
                  keyword: Ref[String],
                  selected: Ref[String],
                  error: Ref[String],
                  reset: Ref[String])
object Colors{

  def Default = Colors(
    Console.MAGENTA,
    Console.CYAN,
    Console.GREEN,
    pprint.Config.Colors.PPrintConfig.colors.literalColor,
    pprint.Config.Colors.PPrintConfig.colors.prefixColor,
    Console.BLUE,
    Console.YELLOW,
    Console.REVERSED,
    Console.RED,
    Console.RESET
  )
  def BlackWhite = Colors("", "", "", "", "", "", "", "", "", "")
}
