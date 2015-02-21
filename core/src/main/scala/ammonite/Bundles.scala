package ammonite
import acyclic.file
import java.nio.file.Files
import java.nio.file.attribute.PosixFileAttributes

import ammonite.ops.{RelPath, Path, Op1}

import scala.collection.{Seq, GenTraversableOnce, TraversableLike}

import scala.util.matching.Regex
object all extends Bundle
object shell extends Bundle{
  implicit var wd = processWorkingDir
  implicit val pathRepr = pprint.PPrinter[ammonite.ops.Path]{ (p, c) =>
    def munge(s: Iterator[String]) = s.flatMap(Iterator("/") ++ BasePath.reprSection(_, c))
    if (p > wd) Iterator("wd") ++ munge((p - wd).segments.iterator)
    else Iterator("root") ++ munge(p.segments.iterator)
  }
  object cd extends Op1[RelPath, Path]{
    def apply(p: RelPath) = {
      wd /= p
      wd
    }
  }
}
trait Bundle extends ops.RelPathStuff with ops.Extensions{

  type BasePath[ThisType <: BasePath[ThisType]] = ops.BasePath[ThisType]
  val BasePath = ops.BasePath
  val PathError = ops.PathError

  type Path = ops.Path
  val Path = ops.Path

  type RelPath = ops.RelPath
  val RelPath = ops.RelPath
  /**
   * The root of the filesystem
   */
  val root = ops.Path.root

  /**
   * The user's home directory
   */
  val home = Path(System.getProperty("user.home"))

  /**
   * The current working directory for this process.
   */
  def processWorkingDir = ops.Path(new java.io.File(""))

  implicit def PathFileData(p: ops.Path) = new ops.FileData(
    Files.readAttributes(java.nio.file.Paths.get(p.toString), classOf[PosixFileAttributes])
  )

  implicit class Transformable1(p: java.nio.file.Path){
    def amm = {
      val s = p.toString

      if (s.startsWith("/")) ops.Path(s)
      else ops.RelPath(s)
    }
  }

  /**
   * Extractor to let you easily pattern match on [[ops.Path]]s
   */
  object /{

    def unapply[T <: BasePath[T]](p: T): Option[(T, String)] = {
      if (p.segments.length > 0)
        Some((p / up, p.last))
      else None
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

  val cp = ops.cp
  val exists = ops.exists
  val grep = ops.grep
  val ln = ops.ln
  val ls = ops.ls
  val mkdir = ops.mkdir
  val mv = ops.mv
  val read = ops.read
  val rm = ops.rm
  val write = ops.write

  val % = ops.%
  type % = ops.%
}
