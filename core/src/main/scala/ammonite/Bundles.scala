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
  object cd extends Op1[RelPath, Path]{
    def apply(p: RelPath) = {
      wd /= p
      wd
    }
  }
}
trait Bundle extends ops.RelPathStuff with ops.Extensions{

  /**
   * The root of the filesystem
   */
  val root = ops.Path.root

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
    def unapply(p: ops.Path): Option[(ops.Path, String)] = {
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
}
