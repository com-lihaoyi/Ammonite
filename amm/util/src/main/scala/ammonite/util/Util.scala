/**
  * Miscellaneous rubbish that isn't big enough to warrant a separate file
  */
package ammonite.util

import java.util.AbstractMap.{SimpleImmutableEntry => Entry}
import java.util.{Map => JMap}
import java.security.MessageDigest




object Util{
  val javaPrefixes = Set("java", "jdk", "javax")
  def lookupWhiteList(whitelist: Set[Seq[String]], tokens: Seq[String]): Boolean = {
    if (whitelist.isEmpty) true
    else {
      tokens.foreach(s => assert(!s.contains('/'), s))
      javaPrefixes.contains(tokens.head) || whitelist(tokens)
    }
  }
  def withContextClassloader[T](contextClassloader: ClassLoader)(t: => T) = {
    val oldClassloader = Thread.currentThread().getContextClassLoader
    try{
      Thread.currentThread().setContextClassLoader(contextClassloader)
      t
    } finally {
      Thread.currentThread().setContextClassLoader(oldClassloader)
    }
  }
  val upPathSegment = "^"
  def pathToPackageWrapper(flexiblePkgName0: Seq[Name],
                           relPath0: os.RelPath): (Seq[Name], Name) = {
    var flexiblePkgName = flexiblePkgName0
    var relPath = relPath0/os.up
    val fileName = relPath0.last
    while(
      flexiblePkgName.length > 1 &&
      flexiblePkgName.last.encoded != upPathSegment &&
      relPath.ups > 0
    ){
      flexiblePkgName = flexiblePkgName.dropRight(1)
      relPath = os.RelPath(relPath.segments, relPath.ups - 1)
    }
    val pkg = {
      val ups = Seq.fill(relPath.ups)(upPathSegment)
      val rest = relPath.segments
      flexiblePkgName ++ (ups ++ rest).map(Name(_))
    }
    val wrapper = fileName.lastIndexOf('.') match{
      case -1 => fileName
      case i => fileName.take(i)
    }

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
  val java9OrAbove = !System.getProperty("java.specification.version").startsWith("1.")
  val newLine = System.lineSeparator()
  // Type aliases for common things

  /**
    * Refers to a wrapper object compiled with a specific set of source code
    * and enclosing environment (encapsulated in the hashVal). This lets us
    * unambiguously look up the correct version of a class, or invalidate and
    * recompile it if the source code or environment changes
    */
  case class VersionedWrapperId(wrapperPath: String,
                                tag: Tag)



  type ClassFiles = Vector[(String, Array[Byte])]


  def encodeFilePath(path: Seq[Name]) = path.map(_.encoded)
  def encodeScalaSourcePath(path: Seq[Name]) = path.map(_.backticked).mkString(".")
  def encodeJvmPath(path: Seq[Name]) = path.map(_.encoded).mkString(".")


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

  def entry[K, V](k: K, v: V): JMap.Entry[K, V] =
    new Entry(k, v)

  def javaIt[T](it: Iterator[T]): java.util.Iterator[T] = {
    import scala.collection.JavaConverters._
    it.asJava
  }

  def isUnit(value: Any): Boolean =
    value == () ||
      // If value was created by a Scala library loaded by a separate ClassLoader
      value.getClass.getName == "scala.runtime.BoxedUnit"
}
