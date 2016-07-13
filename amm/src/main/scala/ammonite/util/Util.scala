/**
  * Miscellaneous rubbish that isn't big enough to warrant a separate file
  */
package ammonite.util

import java.security.MessageDigest
import acyclic.file
import ammonite.ops._
import ammonite.util.Parsers.ImportTree

trait Timer{
  def apply[T](t: => T)(implicit n: sourcecode.Enclosing): T
}
object Timer{
  def none = new Timer{
    def apply[T](t: => T)(implicit n: sourcecode.Enclosing) = t
  }
  def apply() = new Timer{
    var current = 0L

    def reset() = current = System.nanoTime()

    var indents = 0
    def apply[T](t: => T)(implicit n: sourcecode.Enclosing) = {
      val start = System.nanoTime()
      val gap = "    " * indents
      println(gap + "+ " + n.value)
      indents += 1
      val res = t
      indents -= 1
      val end = System.nanoTime
      println(gap + "- " + n.value + ":\t" + (end - start) / 1000000.0)
      res
    }
  }
}

object Util{

  def pathToPackageWrapper(path: Path, wd: Path): (Seq[Name], Name) = {
    val pkg = {
      val base = Seq("$file")
      val relPath = (path/up).relativeTo(wd)
      val ups = Seq.fill(relPath.ups)("..")
      val rest = relPath.segments
      (base ++ ups ++ rest).map(Name(_))
    }
    val wrapper = path.last.take(path.last.lastIndexOf('.'))
    (pkg, Name(wrapper))
  }
  def md5Hash(data: Iterator[Array[Byte]]) = {
    val digest = MessageDigest.getInstance("MD5")
    data.foreach(digest.update)
    digest.digest()
  }

  val windowsPlatform = System.getProperty("os.name").startsWith("Windows")
  // Type aliases for common things

  type CacheDetails = (String, String)
  //                   Wrapper HashVal
  type IvyMap = Map[(String, String, String, String), Set[String]]
  type ClassFiles = Vector[(String, Array[Byte])]
  type CacheOutput = (Seq[String], Seq[ClassFiles], Imports, Seq[ImportTree])
  type CompileCache = (ClassFiles, Imports)


  def transpose[A](xs: List[List[A]]): List[List[A]] = {
    @scala.annotation.tailrec
    def transpose(xs: List[List[A]], result: List[List[A]]): List[List[A]] = {
      xs.filter(_.nonEmpty) match {
        case Nil    =>  result
        case ys: List[List[A]] => transpose(ys.map(_.tail), ys.map(_.head) :: result)
      }
    }

    transpose(xs, Nil).reverse
  }
}