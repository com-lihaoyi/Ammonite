import java.nio.file.Files
import java.nio.file.attribute.PosixFileAttributes

import scala.collection.{Seq, GenTraversableOnce, TraversableLike}
import scala.collection.generic.{CanBuildFrom => CBF, GenericTraversableTemplate, SeqFactory}
import scala.util.matching.Regex

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

  def processWorkingDir = Path(new java.io.File("."))

  implicit def PathFileData(p: Path) = new FileData(
    Files.readAttributes(java.nio.file.Paths.get(p.toString), classOf[PosixFileAttributes])
  )

  implicit def funcWhee[T, T1, V](f: T => V)(implicit i: T1 => T) = i andThen f

  implicit def SeqFactoryFunc[T, CC[X] <: Seq[X] with GenericTraversableTemplate[X, CC]]
                             (s: SeqFactory[CC]) = {
    (t: Seq[T]) => s(t:_*)
  }


  class Interped(parts: Seq[String]){
    def unapplySeq(s: String) = {
      val Seq(head, tail@_*) = parts.map(java.util.regex.Pattern.quote)

      val regex = head + tail.map("(.*)" + _).mkString
      regex.r.unapplySeq(s)
    }
  }
  object /{
    def unapply(p: Path): Option[(Path, String)] = {
      Some((p / up, p.last))
    }
  }
  implicit class RegexContext(sc: StringContext) {
    def r = new Interped(sc.parts)
  }
}
