package ammonite.ops

import scala.collection.generic.{IsIterable, IsSeq}
import scala.collection.{BuildFrom, SeqFactory, SeqOps}
import scala.reflect.ClassTag

/**
 * Extends collections to give short aliases for the commonly
 * used operations, so we can make it easy to use from the
 * command line.
 */
class FilterMapExt[+T, Repr](i: Repr, coll: IsIterable[Repr] { type A = T }) {
  /**
   * Alias for `map`
   */
  def |[B, That](f: T => B)(implicit bf: BuildFrom[Repr, B, That]): That = {
    val b = bf.newBuilder(i)
    b.addAll(coll(i).iterator.map(f))
    b.result()
  }

  /**
   * Alias for `flatMap`
   */
  def ||[B, That](f: T => IterableOnce[B])(implicit bf: BuildFrom[Repr, B, That]): That = {
    val b = bf.newBuilder(i)
    b.addAll(coll(i).iterator.flatMap(f))
    b.result()
  }

  /**
   * Alias for `foreach`
   */
  def |![B, That](f: T => Unit) = coll(i).foreach(f)

  /**
   * Alias for `filter`
   */
  def |?(p: T => Boolean): Seq[T] =
    coll(i).iterator.filter(p).toSeq

  /**
   * Alias for `reduce`
   */
  def |&[A1 >: T](op: (A1, A1) => A1): A1 = coll(i).reduceLeft(op)
}

trait FilterMapExtConv {

  implicit def FilterMapExtImplicit[Repr](repr: Repr)(implicit
    i: IsIterable[Repr]): FilterMapExt[i.A, Repr] =
    new FilterMapExt[i.A, Repr](repr, i)
  /**
   * Lets you call [[FilterMapExt]] aliases on Arrays too
   */
  implicit def FilterMapArraysImplicit[T: ClassTag](a: Array[T]): FilterMapExt[T, Array[T]] =
    new FilterMapExt[T, Array[T]](a, IsSeq.arrayIsSeq[T])

  /**
   * Allows you to pipe sequences into other sequences to convert them,
   * e.g. Seq(1, 2, 3) |> Vector
   */
  implicit def SeqFactoryFunc[T, CC[X] <: Seq[X] with SeqOps[X, CC, CC[X]]]
                             (s: SeqFactory[CC]) = {
    (t: Seq[T]) => s(t:_*)
  }

}