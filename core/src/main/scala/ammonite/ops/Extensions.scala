package ammonite.ops
import acyclic.file
import scala.collection.{Seq, GenTraversableOnce, TraversableLike}

import scala.collection.generic.{CanBuildFrom => CBF, GenericTraversableTemplate, SeqFactory}

trait Extensions{
  implicit def Pipeable[T](t: T) = new Pipeable(t)
  implicit def FilterMapExt[T, Repr](i: TraversableLike[T, Repr]) = new FilterMap(i)
  /**
   * Lets you call [[FilterMapExt]] aliases on Arrays too
   */
  implicit def FMArray[T](a: Array[T]) = FilterMapExt(a)

  /**
   * Allows you to pipe sequences into other sequences to convert them,
   * e.g. Seq(1, 2, 3) |> Vector
   */
  implicit def SeqFactoryFunc[T, CC[X] <: Seq[X] with GenericTraversableTemplate[X, CC]]
  (s: SeqFactory[CC]) = {
    (t: Seq[T]) => s(t:_*)
  }

  implicit def ChainableConversions[T, T1, V](f: T => V)(implicit i: T1 => T) = i andThen f
}

object Extensions extends Extensions

/**
 * Lets you pipe values through functions
 */

class Pipeable[T](t: T){
  def |>[V](f: T => V) = f(t)
}
/**
 * Extends collections to give short aliases for the commonly
 * used operations, so we can make it easy to use from the
 * command line.
 */
class FilterMap[+T, Repr](i: TraversableLike[T, Repr]){
  /**
   * Alias for `map`
   */
  def |[B, That](f: T => B)(implicit bf: CBF[Repr, B, That]): That = i.map(f)

  /**
   * Alias for `flatMap`
   */
  def ||[B, That](f: T => GenTraversableOnce[B])(implicit bf: CBF[Repr, B, That]): That = {
    i.flatMap(f)
  }

  /**
   * Alias for `filter`
   */
  def |?(p: T => Boolean): Repr = i.filter(p)

  /**
   * Alias for `reduce`
   */
  def |&[A1 >: T](op: (A1, A1) => A1): A1 = i.reduceLeft(op)
}