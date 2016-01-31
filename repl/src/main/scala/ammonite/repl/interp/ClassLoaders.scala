package ammonite.repl.interp

import java.io.IOException
import java.net.{URL, URLClassLoader}
import java.nio.file
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{SimpleFileVisitor, FileVisitResult, FileVisitor}
import java.security.MessageDigest

import ammonite.ops._
import ammonite.repl.{ImportData, Util}
import pprint.PPrint

import scala.collection.mutable
import scala.reflect.io.VirtualDirectory

case class Frame(classloader: SpecialClassLoader,
                 pluginClassloader: SpecialClassLoader,
                 var previousImports: Map[String, ImportData],
                 var classpath: Seq[java.io.File])

object SpecialClassLoader{
  val simpleNameRegex = "[a-zA-Z0-9_]+".r

  /**
    * Stats all loose class-files in the current classpath that could
    * conceivably be part of some package, i.e. their directory path
    * doesn't contain any non-package-identifier segments, and aggregates
    * their names and mtimes as a "signature" of the current classpath
    */
  def initialClasspathHash(classloader: ClassLoader): Array[Byte] = {
    val allClassloaders = {
      val all = mutable.Buffer.empty[ClassLoader]
      var current = classloader
      while(current != null){
        all.append(current)
        current = current.getParent
      }
      all
    }

    def findMtimes(d: file.Path): Seq[(Path, Long)] = {
      def skipSuspicious(path: Path) = {
        simpleNameRegex.findPrefixOf(path.last) == Some(path.last)
      }
      ls.rec(skip = skipSuspicious)! Path(d) | (x => (x, x.mtime.toMillis))
    }

    val classpathFolders =
      allClassloaders.collect{case cl: java.net.URLClassLoader => cl.getURLs}
                     .flatten
                     .filter(_.getProtocol == "file")
                     .map(_.toURI)
                     .map(java.nio.file.Paths.get)
                     .filter(java.nio.file.Files.isDirectory(_))

    val classFileMtimes = classpathFolders.flatMap(f => findMtimes(f))

    val hashes = classFileMtimes.map{ case (name, mtime) =>
      Util.md5Hash(Iterator(
        name.toString.getBytes,
        (0 until 64 by 8).iterator.map(mtime >> _).map(_.toByte).toArray
      ))
    }
    Util.md5Hash(hashes.iterator)
  }
}
/**
  * Classloader used to implement the jar-downloading
  * command-evaluating logic in Ammonite.
  *
  * http://stackoverflow.com/questions/3544614/how-is-the-control-flow-to-findclass-of
  */
class SpecialClassLoader(parent: ClassLoader, parentHash: Array[Byte])
  extends URLClassLoader(Array(), parent){
  /**
    * Files which have been compiled, stored so that our special
    * classloader can get at them.
    */
  val newFileDict = mutable.Map.empty[String, Array[Byte]]
  def findClassPublic(name: String) = findClass(name)
  val specialLocalClasses = Set(
    "ammonite.repl.frontend.ReplBridge",
    "ammonite.repl.frontend.ReplBridge$"
  )
  override def findClass(name: String): Class[_] = {
    def loadedFromBytes =
      for(bytes <- newFileDict.get(name))
        yield defineClass(name, bytes, 0, bytes.length)

    def special =
      if (!specialLocalClasses(name)) None
      else{
        import ammonite.ops._
        //          println("Custom finding class! " + name)
        val bytes = read.resource.bytes(root/RelPath(name.replace('.', '/') + ".class"))
        Some(defineClass(name, bytes, 0, bytes.length))
      }

    Option(this.findLoadedClass(name))
      .orElse(loadedFromBytes)
      .orElse(special)
      .getOrElse(super.findClass(name))
  }
  def add(url: URL) = {
    _classpathHash = Util.md5Hash(Iterator(_classpathHash, jarHash(url)))
    addURL(url)
  }

  private def jarHash(url: URL) = {
    val digest = MessageDigest.getInstance("MD5")
    val is = url.openStream
    try {
      val byteChunk = new Array[Byte](8192)
      while({
        val n = is.read(byteChunk)
        if (n <= 0) false
        else {
          digest.update(byteChunk, 0, n)
          true
        }
      })()
    } finally {
      if (is != null) is.close()
    }

    digest.digest()
  }

  def initialClasspathHash = parentHash
  private[this] var _classpathHash = initialClasspathHash
  def classpathHash: Array[Byte] = _classpathHash
  def allJars: Seq[URL] = {
    this.getURLs ++ ( parent match{
      case t: SpecialClassLoader => t.allJars
      case _ => Nil
    })
  }
}
