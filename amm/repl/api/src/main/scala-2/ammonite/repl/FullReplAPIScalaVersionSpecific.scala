package ammonite.repl

import scala.reflect.runtime.universe._

trait FullReplAPIScalaVersionSpecific {

  def typeOf[T: WeakTypeTag] = scala.reflect.runtime.universe.weakTypeOf[T]
  def typeOf[T: WeakTypeTag](t: => T) = scala.reflect.runtime.universe.weakTypeOf[T]

}
