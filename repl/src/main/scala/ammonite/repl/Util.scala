package ammonite.repl

import acyclic.file

import scala.tools.nsc.Global
import scala.util.Try

object Result{
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
  case class Success[+T](s: T) extends Result[T] {
    def flatMap[V](f: T => Result[V]): Result[V] = f(s) match {
      case Success(v) => Success(v)
      case other => other
    }

    def map[V](f: T => V): Result[V] = Success(f(s))
  }

  /**
   * Failing results never call their callbacks, and just remain unchanged
   */
  sealed abstract class Failing extends Result[Nothing]{
    def flatMap[V](f: Nothing => Result[V]): Result[V] = this
    def map[V](f: Nothing => V): Result[V] = this
  }
  case class Failure(s: String) extends Failing
  object Failure{
    def apply(exceptions: Seq[Throwable], stop: String = null): Failure = {
      val traces = exceptions.map(exception =>
        exception.toString + "\n" +
        exception
          .getStackTrace
          .takeWhile(x => !(x.getMethodName == stop))
          .map("\t" + _)
          .mkString("\n")
      )
      Result.Failure(traces.mkString("\n"))
    }
  }
  case object Skip extends Failing
  case object Exit extends Failing
  case class Buffer(s: String) extends Failing
}

/**
 * Represents the output of a single pass through the ammonite REPL.
 */
sealed abstract class Result[+T]{
  def flatMap[V](f: T => Result[V]): Result[V]
  def map[V](f: T => V): Result[V]
  def filter(f: T => Boolean): Result[T] = this
}

/**
 * Fake for-comprehension generator to catch errors and turn
 * them into [[Result.Failure]]s
 */
case class Catching(handler: PartialFunction[Throwable, Result.Failing]) {

  def foreach[T](t: Unit => T): T = t(())
  def flatMap[T](t: Unit => Result[T]): Result[T] =
    try{t(())} catch handler
  def map[T](t: Unit => T): Result[T] =
    try Result.Success(t(())) catch handler
}

case class Evaluated(wrapper: String,
                     imports: Seq[ImportData])

case class ImportData(imported: String, wrapperName: String, prefix: String)

/**
 * The results of parsing a string into code.
 *
 * Note that error cases don't convey any additional information;
 * any debugging would already have been printed by the compiler
 * to stdout.
 */
abstract sealed class Parsed
object Parsed{
  case class Error(msg: String) extends Parsed
  case object Incomplete extends Parsed
  case class Success(trees: List[Global#Tree]) extends Parsed
}

trait Ref[T]{
  def apply(): T
  def update(t: T): Unit
}
object Ref{
  def apply[T](value0: T) = {
    var value = value0
    new Ref[T]{
      def apply() = value
      def update(t: T) = value = t
    }
  }
  def apply[T](value: T, update0: T => Unit) = new Ref[T]{
    def apply() = value
    def update(t: T) = update0(t)
  }
}

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