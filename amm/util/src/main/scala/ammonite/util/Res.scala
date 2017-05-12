package ammonite.util

import acyclic.file
import scala.language.higherKinds
import scala.util.Try


/**
  * The result of a single pass through the ammonite REPL. Results in a single
  * [[T]], or of one of a fixed number of failures that are common for anyone
  * who is evaluating code via the REPL.
  */
sealed abstract class Res[+T]{
  def flatMap[V](f: T => Res[V]): Res[V]
  def map[V](f: T => V): Res[V]
  def filter(f: T => Boolean): Res[T] = this
  def withFilter(f: T => Boolean): Res[T] = filter(f)
}



object Res{
  /**
    * Maps a Res-returning function across a collection `M[T]`, failing fast and
    * bailing out if any individual element fails.
    */
  def map[M[_] <: Traversable[_], T, V](inputs: M[T])(f: T => Res[V])
                                       (implicit cbf: collection.generic.CanBuildFrom[_, V, M[V]])
  : Res[M[V]] = {
    val builder = cbf.apply()

    inputs.foldLeft[Res[Unit]](Res.Success(())){
      case (f: Failing, _) => f
      case (Res.Success(prev), x: T) =>
        f(x) match{
          case f: Failing => f
          case Success(x) =>
            builder += x
            Res.Success(())
        }
    } match{
      case Success(_) => Res.Success(builder.result())
      case f: Failing => f
    }
  }
  def apply[T](o: Option[T], errMsg: => String) = o match{
    case Some(s) => Success(s)
    case None => Failure(None, errMsg)
  }
  def apply[T](o: Try[T], errMsg: Throwable => String) = o match{
    case util.Success(s) => Success(s)
    case util.Failure(t) => Failure(None, errMsg(t))
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

  /**
    * A known failure occured, maybe caused by an exception
    * (e.g. `ThreadDeath`) and maybe not (e.g. compile error)
    *
    * @param ex is any exception that caused this known failure; currently
    *           only used for the "Interrupted!" failures caused by Ctrl-C
    * @param msg the message we want to display on screen due to this failure
    */
  case class Failure(ex: Option[Throwable], msg: String) extends Failing

  /**
    * An unknown exception was thrown when the command was being run
    */
  case class Exception(t: Throwable, s: String) extends Failing

  /**
    * Nothing was entered
    */
  case object Skip extends Failing

  /**
    * The user wanted to exit the REPL
    */
  case class Exit(value: Any) extends Failing
}


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
