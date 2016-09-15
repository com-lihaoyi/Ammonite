package ammonite.kernel

import ammonite.ops._
import collection.mutable
import java.net.{URL, URLClassLoader}


/**
  * Classloader used to implement the jar-downloading command-evaluating logic in Ammonite.
  *
  * http://stackoverflow.com/questions/3544614/how-is-the-control-flow-to-findclass-of
  */
private[kernel] final class AmmoniteClassLoader(parent: ClassLoader, parentSignature: Seq[(Path, Long)])
    extends URLClassLoader(Array(), parent) {

  /**
    * Files which have been compiled, stored so that our special
    * classloader can get at them.
    */
  private val newFileDict: mutable.Map[String, Array[Byte]] = mutable.Map.empty

  def addClassFile(name: String, bytes: Array[Byte]): Unit = {
    val tuple = Path(name, root) -> bytes.sum.hashCode().toLong
    classpathSignature0 = classpathSignature0 ++ Seq(tuple)
    newFileDict(name) = bytes
  }

  override def findClass(name: String): Class[_] = {
    val loadedClass = this.findLoadedClass(name)
    if (loadedClass != null) {
      loadedClass
    } else if (newFileDict.contains(name)) {
      val bytes = newFileDict(name)
      defineClass(name, bytes, 0, bytes.length)
    } else {
      super.findClass(name)
    }
  }

  def add(url: URL): Unit = {
    classpathSignature0 = classpathSignature0 ++ Seq(jarSignature(url))
    addURL(url)
  }

  private def jarSignature(url: URL) = {
    val path = Path(java.nio.file.Paths.get(url.toURI()).toFile(), root)
    path -> path.mtime.toMillis
  }

  private[this] var classpathSignature0 = parentSignature

  private def allJars: Seq[URL] = {
    this.getURLs ++ (parent match {
      case t: AmmoniteClassLoader => t.allJars
      case _ => Nil
    })
  }
}

private[kernel] object AmmoniteClassLoader {

  private val simpleNameRegex = "[a-zA-Z0-9_]+".r

  /**
    * Stats all loose class-files in the current classpath that could
    * conceivably be part of some package, i.e. their directory path
    * doesn't contain any non-package-identifier segments, and aggregates
    * their names and mtimes as a "signature" of the current classpath
    */
  def initialClasspathSignature(classloader: ClassLoader): Seq[(Path, Long)] = {
    val allClassloaders = {
      val all = mutable.Buffer.empty[ClassLoader]
      var current = classloader
      while (current != null) {
        all.append(current)
        current = current.getParent
      }
      all
    }

    def findMtimes(d: java.nio.file.Path): Seq[(Path, Long)] = {
      def skipSuspicious(path: Path) = {
        simpleNameRegex.findPrefixOf(path.last) == Some(path.last)
      }
      ls.rec(skip = skipSuspicious) ! Path(d) | (x => (x, x.mtime.toMillis))
    }

    val classpathFolders =
      allClassloaders.collect {
        case cl: java.net.URLClassLoader => cl.getURLs
      }.flatten
        .filter(_.getProtocol == "file")
        .map(_.toURI)
        .map(java.nio.file.Paths.get)
        .filter(java.nio.file.Files.isDirectory(_))

    val classFileMtimes = classpathFolders.flatMap(f => findMtimes(f))
    classFileMtimes

  }
}
