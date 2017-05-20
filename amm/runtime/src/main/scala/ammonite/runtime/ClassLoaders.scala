package ammonite.runtime

import java.io.ByteArrayInputStream
import java.net.{URL, URLClassLoader, URLConnection, URLStreamHandler}
import java.nio.ByteBuffer
import java.util.{Collections, Enumeration}


import ammonite.ops._
import ammonite.util.{Imports, Util}

import scala.collection.mutable




/**
  * Represents a single "frame" of the `sess.save`/`sess.load` stack/tree.
  *
  * Exposes `imports` and `classpath` as readable but only writable
  * in particular ways: `imports` can only be updated via `mergeImports`,
  * while `classpath` can only be added to.
  */
class Frame(val classloader: SpecialClassLoader,
            val pluginClassloader: SpecialClassLoader,
            private[this] var imports0: Imports,
            private[this] var classpath0: Seq[java.io.File]){
  def imports = imports0
  def classpath = classpath0
  def addImports(additional: Imports) = {
    imports0 = imports0 ++ additional
  }
  def addClasspath(additional: Seq[java.io.File]) = {
    additional.map(_.toURI.toURL).foreach(classloader.add)
    classpath0 = classpath0 ++ additional
  }
}

object SpecialClassLoader{
  val simpleNameRegex = "[a-zA-Z0-9_]+".r

  /**
    * Stats all jars on the classpath, and loose class-files in the current
    * classpath that could conceivably be part of some package, and aggregates
    * their names and mtimes as a "signature" of the current classpath
    *
    * When looking for loose class files, we skip folders whose names are not
    * valid java identifiers. Otherwise, the "current classpath" often contains
    * the current directory, which in an SBT or Maven project contains hundreds
    * or thousands of files which are not on the classpath. Empirically, this
    * heuristic improves perf by greatly cutting down on the amount of files we
    * need to mtime in many common cases.
    */
  def initialClasspathSignature(classloader: ClassLoader): Seq[(Path, Long)] = {
    val allClassloaders = {
      val all = mutable.Buffer.empty[ClassLoader]
      var current = classloader
      while(current != null){
        all.append(current)
        current = current.getParent
      }
      all
    }

    def findMtimes(d: java.nio.file.Path): Seq[(Path, Long)] = {
      def skipSuspicious(path: Path) = {
        // Leave out sketchy files which don't look like package names or
        // class files
        (simpleNameRegex.findPrefixOf(path.last) != Some(path.last)) &&
        !path.last.endsWith(".class")
      }
      ls.rec(skip = skipSuspicious)! Path(d) | (x => (x, x.mtime.toMillis))
    }


    val classpathRoots =
      allClassloaders
        .collect{case cl: java.net.URLClassLoader => cl.getURLs}
        .flatten
        .filter(_.getProtocol == "file")

    val mtimes = classpathRoots.flatMap{ rawP =>
      val p = java.nio.file.Paths.get(rawP.toURI)
      if (!java.nio.file.Files.exists(p)) {
        None
      }
      else if (java.nio.file.Files.isDirectory(p)){
        Some(findMtimes(p))
      }
      else  {
        Some(Seq(Path(p) -> Path(p).mtime.toMillis))
      }
    }.flatten

    mtimes
  }
}
/**
  * Classloader used to implement the jar-downloading
  * command-evaluating logic in Ammonite.
  *
  * http://stackoverflow.com/questions/3544614/how-is-the-control-flow-to-findclass-of
  */
class SpecialClassLoader(parent: ClassLoader, parentSignature: Seq[(Path, Long)])
  extends URLClassLoader(Array(), parent){

  /**
    * Files which have been compiled, stored so that our special
    * classloader can get at them.
    */
  val newFileDict = mutable.Map.empty[String, Array[Byte]]
  def addClassFile(name: String, bytes: Array[Byte]) = {
    val tuple = Path(name, root) -> bytes.sum.hashCode().toLong
    classpathSignature0 = classpathSignature0 ++ Seq(tuple)
    newFileDict(name) = bytes
  }
  def findClassPublic(name: String) = findClass(name)
  val specialLocalClasses = Set(
    "ammonite.repl.ReplBridge",
    "ammonite.repl.ReplBridge$",
    "ammonite.interp.InterpBridge",
    "ammonite.interp.InterpBridge$"
  )
  override def findClass(name: String): Class[_] = {
    val loadedClass = this.findLoadedClass(name)
    if (loadedClass != null) loadedClass
    else if (newFileDict.contains(name)) {
      val bytes = newFileDict(name)
      defineClass(name, bytes, 0, bytes.length)
    }else if (specialLocalClasses(name)) {
      import ammonite.ops._
      val resource = this.getResourceAsStream(name.replace('.', '/') + ".class")
      if (resource != null){
        val bytes = read.bytes(resource)

        defineClass(name, bytes, 0, bytes.length)
      }else{
        super.findClass(name)
      }

    } else super.findClass(name)
  }
  def add(url: URL) = {
    classpathSignature0 = classpathSignature0 ++ Seq(jarSignature(url))
    this.addURL(url)
  }

  override def close() = {
    // DO NOTHING LOLZ

    // Works around
    // https://github.com/scala/scala/commit/6181525f60588228ce99ab3ef2593ecfcfd35066
    // Which for some reason started mysteriously closing these classloaders in 2.12
  }

  private def jarSignature(url: URL) = {
    val path = Path(java.nio.file.Paths.get(url.toURI()).toFile(), root)
    path -> (if (exists(path))path.mtime.toMillis else 0)
  }

  private[this] var classpathSignature0 = parentSignature
  def classpathSignature = classpathSignature0
  def classpathHash = {
    Util.md5Hash(
      classpathSignature0.iterator.map { case (path, long) =>
        val buffer = ByteBuffer.allocate(8)
        buffer.putLong(long)
        path.toString.getBytes ++ buffer.array()
      }
    )

  }
  def allJars: Seq[URL] = {
    this.getURLs ++ ( parent match{
      case t: SpecialClassLoader => t.allJars
      case _ => Nil
    })
  }

  override def findResource(name: String) = {
    getURLFromFileDict(name).getOrElse(super.findResource(name))
  }

  override def findResources(name: String) = getURLFromFileDict(name) match {
    case Some(u) => Collections.enumeration(Collections.singleton(u))
    case None    => super.findResources(name)
  }

  private def getURLFromFileDict(name: String) = {
    val className = name.stripSuffix(".class").replace('/', '.')
    newFileDict.get(className) map { x =>
      new URL(null, s"memory:${name}", new URLStreamHandler {
        override def openConnection(url: URL): URLConnection = new URLConnection(url) {
          override def connect() = ()
          override def getInputStream = new ByteArrayInputStream(x)
        }
      })
    }
  }

}
