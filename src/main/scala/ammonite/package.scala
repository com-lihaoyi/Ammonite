import scala.collection.{GenTraversableOnce, TraversableLike}
import scala.collection.generic.{CanBuildFrom => CBF}

package object ammonite extends RelPathStuff{
  type T[A] = Iterator[A]
  /**
   * Extends collections to give short aliases for the commonly
   * used operations, so we can make it easy to use from the
   * command line.
   */
  implicit class FilterMap[+T, Repr](i: TraversableLike[T, Repr]){
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

  implicit class Pipeable[T](t: T){
    def |>[V](f: T => V) = f(t)
  }
  val root = Path.root

  def cwd = Path(new java.io.File("."))
  implicit def PathFileData(p: Path) = FileData.make(meta! p)

  implicit def funcWhee[T, T1, V](f: T => V)(implicit i: T1 => T) = i andThen f
}
