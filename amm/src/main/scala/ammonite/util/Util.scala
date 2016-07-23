/**
  * Miscellaneous rubbish that isn't big enough to warrant a separate file
  */
package ammonite.util

import java.security.MessageDigest
import acyclic.file
import ammonite.ops._
import ammonite.util.Parsers.ImportTree


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

  //normalizes strings to have new line of the OS program is being run on
  //irrespective of the OS on which script was written
  def normalizeNewlines(s: String) = s.replace("\r", "").replace("\n", newLine)


  val windowsPlatform = System.getProperty("os.name").startsWith("Windows")
  val newLine = System.lineSeparator()
  // Type aliases for common things

  type CacheDetails = (String, String)
  //                   Wrapper HashVal
  type IvyMap = Map[(String, String, String, String), Set[String]]
  type ClassFiles = Vector[(String, Array[Byte])]
  type CacheOutput = (Seq[(String, String)], Seq[ClassFiles], Imports, Seq[ImportTree])
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