package ammonite.ops

import scala.collection.{Seq, GenTraversableOnce, TraversableLike}

import scala.collection.generic.{CanBuildFrom => CBF, GenericTraversableTemplate, SeqFactory}

/**
 * Extends collections to give short aliases for the commonly
 * used operations, so we can make it easy to use from the
 * command line.
 */
class FilterMapExt[+T, Repr](i: TraversableLike[T, Repr]) {
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
   * Alias for `foreach`
   */
  def |![B, That](f: T => Unit) = i.foreach(f)

  /**
   * Alias for `filter`
   */
  def |?(p: T => Boolean): Repr = i.filter(p)

  /**
   * Alias for `reduce`
   */
  def |&[A1 >: T](op: (A1, A1) => A1): A1 = i.reduceLeft(op)
}

trait FilterMapExtConv {

  implicit def FilterMapExtImplicit[T, Repr](i: TraversableLike[T, Repr]): FilterMapExt[T, Repr] =
    new FilterMapExt(i)
  /**
   * Lets you call [[FilterMapExt]] aliases on Arrays too
   */
  implicit def FilterMapArraysImplicit[T](a: Array[T]): FilterMapExt[T, Array[T]] =
    new FilterMapExt(a)

  /**
   * Allows you to pipe sequences into other sequences to convert them,
   * e.g. Seq(1, 2, 3) |> Vector
   */
  implicit def SeqFactoryFunc[T, CC[X] <: Seq[X] with GenericTraversableTemplate[X, CC]]
                             (s: SeqFactory[CC]) = {
    (t: Seq[T]) => s(t:_*)
  }

}