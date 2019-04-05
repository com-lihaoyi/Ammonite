package ammonite.ops

import scala.collection.GenTraversableOnce

trait Extensions extends FilterMapExtConv{
  implicit def PipeableImplicit[T](t: T): Pipeable[T] = new Pipeable(t)

  implicit def FilterMapIteratorsImplicit[T](a: Iterator[T]): FilterMapExt2[T] =
    new FilterMapExt2(a)

  implicit def FilterMapGeneratorsImplicit[T](a: geny.Generator[T]): FilterMapExtGen[T] =
    new FilterMapExtGen(a)


  implicit class iterShow[T](t: Iterator[T]){
    def !! = t.foreach(println)
  }

  implicit def RegexContextMaker(s: StringContext): RegexContext = new RegexContext(s)

  implicit def Callable1Implicit[T1, R](f: (T1 => R)): Callable1[T1, R] = new Callable1(f)
}

/**
  * Provides `a.! b` as an alternative to the `a(b)` syntax for calling a
  * function with one argument
  */
class Callable1[T1, R](f: (T1 => R)) extends (T1 => R){
  def !(arg: T1): R = f(arg)
  def apply(arg: T1): R = f(arg)
}

/**
  * Provides `a! b` as an alternative to the `(a(b, _)` syntax for partially
  * applying a function with two arguments
  */
class Callable2[T1, T2, R](f: (T1, T2) => R) extends ((T1, T2) => R){
  def !(arg1: T1) = new Callable1[T2, R](arg2 => f(arg1, arg2))
  def apply(arg1: T1, arg2: T2) = f(arg1, arg2)
}
object Extensions extends Extensions

/**
 * Lets you pipe values through functions
 */

class Pipeable[T](t: T) {
  def |>[V](f: T => V) = f(t)
}
/**
 * Extends collections to give short aliases for the commonly
 * used operations, so we can make it easy to use from the
 * command line.
 */
class FilterMapExt2[+T](i: Iterator[T]) {
  /**
   * Alias for `map`
   */
  def |[B, That](f: T => B) = i.map(f)

  /**
   * Alias for `flatMap`
   */
  def ||[B, That](f: T => GenTraversableOnce[B]) = i.flatMap(f)

  /**
   * Alias for `foreach`
   */
  def |![B, That](f: T => Unit) = i.foreach(f)

  /**
   * Alias for `filter`
   */
  def |?(p: T => Boolean) = i.filter(p)

  /**
   * Alias for `reduce`
   */
  def |&[A1 >: T](op: (A1, A1) => A1): A1 = i.reduceLeft(op)
}
/**
 * Extends collections to give short aliases for the commonly
 * used operations, so we can make it easy to use from the
 * command line.
 */
class FilterMapExtGen[+T](i: geny.Generator[T]) {
  /**
   * Alias for `map`
   */
  def |[B, That](f: T => B) = i.map(f)

  /**
   * Alias for `flatMap`
   */
  def ||[B, That](f: T => Iterable[B]) = i.flatMap(f(_))

  /**
   * Alias for `foreach`
   */
  def |![B, That](f: T => Unit) = i.foreach(f)

  /**
   * Alias for `filter`
   */
  def |?(p: T => Boolean) = i.filter(p)

  /**
   * Alias for `reduce`
   */
  def |&[A1 >: T](op: (A1, A1) => A1): A1 = i.reduceLeft(op)
}

object RegexContext{
  class Interped(parts: Seq[String]){
    def unapplySeq(s: String) = {
      val Seq(head, tail@_*) = parts.map(java.util.regex.Pattern.quote)

      val regex = head + tail.map("(.*)" + _).mkString
      regex.r.unapplySeq(s)
    }
  }
}

/**
 * Lets you pattern match strings with interpolated glob-variables
 */
class RegexContext(sc: StringContext) {
  def r = new RegexContext.Interped(sc.parts)
}