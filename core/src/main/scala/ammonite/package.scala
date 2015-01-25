import acyclic.file
import java.nio.file.Files
import java.nio.file.attribute.PosixFileAttributes

import scala.collection.{Seq, GenTraversableOnce, TraversableLike}
import scala.collection.generic.{CanBuildFrom => CBF, GenericTraversableTemplate, SeqFactory}
import scala.util.matching.Regex

package object ammonite extends RelPathStuff{

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

  /**
   * Lets you call [[FilterMap]] aliases on Arrays too
   */
  implicit def FMArray[T](a: Array[T]) = FilterMap(a)

  /**
   * Lets you pipe values through functions
   */
  implicit class Pipeable[T](t: T){
    def |>[V](f: T => V) = f(t)
  }

  /**
   * The root of the filesystem
   */
  val root = Path.root

  val rel = RelPath.rel
  /**
   * The current working directory for this process.
   */
  def processWorkingDir = Path(new java.io.File(""))

  implicit def PathFileData(p: Path) = new FileData(
    Files.readAttributes(java.nio.file.Paths.get(p.toString), classOf[PosixFileAttributes])
  )

  implicit def funcWhee[T, T1, V](f: T => V)(implicit i: T1 => T) = i andThen f

  /**
   * Allows you to pipe sequences into other sequences to convert them,
   * e.g. Seq(1, 2, 3) |> Vector
   */
  implicit def SeqFactoryFunc[T, CC[X] <: Seq[X] with GenericTraversableTemplate[X, CC]]
                             (s: SeqFactory[CC]) = {
    (t: Seq[T]) => s(t:_*)
  }


  implicit class Transformable1(p: java.nio.file.Path){
    def amm = {
      val s = p.toString

      if (s.startsWith("/")) Path(s)
      else RelPath(s)
    }
  }


  /**
   * Extractor to let you easily pattern match on [[Path]]s
   */
  object /{
    def unapply(p: Path): Option[(Path, String)] = {
      Some((p / up, p.last))
    }
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
  implicit class RegexContext(sc: StringContext) {
    def r = new RegexContext.Interped(sc.parts)
  }
}
