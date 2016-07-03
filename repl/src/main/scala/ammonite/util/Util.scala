/**
  * Miscellaneous rubbish that isn't big enough to warrant a separate file
  */
package ammonite.util

import java.security.MessageDigest
import acyclic.file
import ammonite.ops._
import ammonite.util.Parsers.ImportTree


object Timer{
  var current = 0L
  var show = false
  def reset() = current = System.nanoTime()
  /**
    * Prints the time, in millis, that has passed
    * since the last time `reset` or `apply` was called
    */
  def apply(s: String) = {
    val now = System.nanoTime()
    if(show) println(s + ": " + (now - current) / 1000000.0)
    current = now
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

  // Type aliases for common things

  type CacheDetails = (String, String)
  //                   Wrapper HashVal
  type IvyMap = Map[(String, String, String, String), Set[String]]
  type ClassFiles = Traversable[(String, Array[Byte])]
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