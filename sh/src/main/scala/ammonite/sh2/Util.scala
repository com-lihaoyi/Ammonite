package ammonite.sh2

import acyclic.file

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
}

/**
 * Represents an
 * @tparam T
 */
sealed abstract class Result[+T]{
  def flatMap[V](f: T => Result[V]): Result[V]
  def map[V](f: T => V): Result[V]
  def filter(f: T => Boolean): Result[T] = this
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
case object Exit extends Failing
