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
case class Catching(handler: PartialFunction[Throwable, String]) {

  def foreach[T](t: Unit => T): T = t(())
  def flatMap[T](t: Unit => Result[T]): Result[T] =
    try{t(())} catch handler.andThen(Result.Failure)
  def map[T](t: Unit => T): Result[T] =
    try Result.Success(t(())) catch handler.andThen(Result.Failure)
}

case class Evaluated(msg: String,
                     wrapper: String,
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
