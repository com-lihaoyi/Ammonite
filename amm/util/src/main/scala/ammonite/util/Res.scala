package ammonite.util


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

  final def isSuccess: Boolean = this match {
    case _: Res.Success[T] => true
    case _ => false
  }
}



object Res{
  def fold[M[_] <: Traversable[_], T, V]
          (init: V, inputs: M[T])(f: (V, T) => Res[V]): Res[V] = {
    inputs.foldLeft[Res[V]](Res.Success(init)){
      case (f: Failing, _) => f
      case (Res.Success(prev), x: T) => f(prev, x)
    }
  }
  /**
    * Maps a Res-returning function across a collection `M[T]`, failing fast and
    * bailing out if any individual element fails.
    */
  def map[M[_] <: Traversable[_], T, V](inputs: M[T])(f: T => Res[V])
                                       (implicit cbf: collection.generic.CanBuildFrom[_, V, M[V]])
  : Res[M[V]] = {

    fold(cbf.apply(), inputs){ (b, v) => f(v).map(b += _) }.map(_.result())
  }
  def apply[T](o: Option[T], errMsg: => String) = o match{
    case Some(s) => Success(s)
    case None => Failure(errMsg)
  }
  def apply[T](o: Either[String, T]) = o match{
    case Right(s) => Success(s)
    case Left(s) => Failure(s)
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

  /**
    * A known failure occured, maybe caused by an exception
    * (e.g. `ThreadDeath`) and maybe not (e.g. compile error)
    *
    * @param msg the message we want to display on screen due to this failure
    */
  case class Failure(msg: String) extends Failing

  /**
    * An unknown exception was thrown when the command was being run,
    * whether from the command itself or within Ammonite's own machinery.
    *
    * Contains an optional [[msg]] that will be printed together with the
    * exception when it it shown to the user.
    */
  case class Exception(t: Throwable, msg: String) extends Failing

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
