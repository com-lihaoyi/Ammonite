package ammonite.runtime

import java.io.ByteArrayInputStream
import java.net.{URL, URLClassLoader, URLConnection, URLStreamHandler}
import java.nio.ByteBuffer
import java.util.Collections


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
            private[this] var classpath0: Seq[java.io.File],
            private[this] var usedEarlierDefinitions0: Seq[String]){
  private var frozen0 = false
  def frozen = frozen0
  def freeze(): Unit = {
    /*
     * Once frozen, a frame won't accept new imports or classpath elements.
     * This is useful in commands that load / save sessions, whose result
     * doesn't need to be kept in the frame.
     */
    frozen0 = true
  }
  private[this] var version0: Int = 0
  def version = version0
  def imports = imports0
  def classpath = classpath0
  def usedEarlierDefinitions = usedEarlierDefinitions0
  def addImports(additional: Imports) = {
    if (!frozen0) {
      version0 += 1
      imports0 = imports0 ++ additional
    }
  }
  def addClasspath(additional: Seq[java.io.File]) = {
    if (!frozen0) {
      version0 += 1
      additional.map(_.toURI.toURL).foreach(classloader.add)
      classpath0 = classpath0 ++ additional
    }
  }
  def addPluginClasspath(additional: Seq[java.io.File]) = {
    if (!frozen0) {
      version0 += 1
      additional.map(_.toURI.toURL).foreach(pluginClassloader.add)
    }
  }
  def usedEarlierDefinitions_=(usedEarlierDefinitions: Seq[String]): Unit =
    usedEarlierDefinitions0 = usedEarlierDefinitions
}
object Frame{
  def createInitial(baseClassLoader: ClassLoader = Thread.currentThread().getContextClassLoader) = {

    // *Try* to load the JVM source files and make them available as resources,
    // so that the `source` helper can navigate to the sources within the
    // Java standard library

    val likelyJdkSourceLocation = Path(System.getProperty("java.home"))/up/"src.zip"
    val hash = SpecialClassLoader.initialClasspathSignature(baseClassLoader)
    def special = new SpecialClassLoader(
      new ForkClassLoader(baseClassLoader, getClass.getClassLoader),
      hash,
      likelyJdkSourceLocation.toNIO.toUri.toURL
    )

    new Frame(special, special, Imports(), Seq(), Seq())
  }
}

case class SessionChanged(removedImports: Set[scala.Symbol],
                          addedImports: Set[scala.Symbol],
                          removedJars: Set[java.net.URL],
                          addedJars: Set[java.net.URL])
object SessionChanged{

  def delta(oldFrame: Frame, newFrame: Frame): SessionChanged = {
    def frameSymbols(f: Frame) = f.imports.value.map(_.toName.backticked).map(Symbol(_)).toSet
    new SessionChanged(
      frameSymbols(oldFrame) -- frameSymbols(newFrame),
      frameSymbols(newFrame) -- frameSymbols(oldFrame),
      oldFrame.classloader.allJars.toSet -- newFrame.classloader.allJars.toSet,
      newFrame.classloader.allJars.toSet -- oldFrame.classloader.allJars.toSet
    )
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
  def initialClasspathSignature(classloader: ClassLoader): Seq[(Either[String, Path], Long)] = {
    val allClassloaders = {
      val all = mutable.Buffer.empty[ClassLoader]
      var current = classloader
      while(current != null && current != ClassLoader.getSystemClassLoader){
        all.append(current)
        current = current.getParent
      }
      all
    }

    def findMtimes(d: java.nio.file.Path): Seq[(Either[String, Path], Long)] = {
      def skipSuspicious(path: Path) = {
        // Leave out sketchy files which don't look like package names or
        // class files
        (simpleNameRegex.findPrefixOf(path.last) != Some(path.last)) &&
        !path.last.endsWith(".class")
      }
      ls.rec(skip = skipSuspicious)! Path(d) | (x => (Right(x), x.mtime.toMillis))
    }


    val classpathRoots =
      allClassloaders
        .collect{case cl: java.net.URLClassLoader => cl.getURLs}
        .flatten
        .filter(_.getProtocol == "file")
        .map(p => java.nio.file.Paths.get(p.toURI))

    val bootClasspathRoots = sys.props("java.class.path")
      .split(java.io.File.pathSeparator)
      .map(java.nio.file.Paths.get(_))

    val mtimes = (bootClasspathRoots ++ classpathRoots).flatMap{ p =>
      if (!java.nio.file.Files.exists(p)) Nil
      else if (java.nio.file.Files.isDirectory(p)) findMtimes(p)
      else Seq(Right(Path(p)) -> Path(p).mtime.toMillis)
    }

    mtimes
  }
}

/**
  * Try to load resources from two parents; necessary to get Ammonite's source
  * code browsing to work in SBT projects because SBT messes up the context
  * classloader https://stackoverflow.com/q/44237791/871202
  */
class ForkClassLoader(realParent: ClassLoader, fakeParent: ClassLoader)
  extends ClassLoader(realParent){
  // This delegates to the parent automatically
  override def findResource(name: String) = fakeParent.getResource(name)
}

/**
  * Classloader used to implement the jar-downloading
  * command-evaluating logic in Ammonite.
  *
  * http://stackoverflow.com/questions/3544614/how-is-the-control-flow-to-findclass-of
  */
class SpecialClassLoader(parent: ClassLoader,
                         parentSignature: Seq[(Either[String, Path], Long)],
                         urls: URL*)
  extends URLClassLoader(urls.toArray, parent){

  /**
    * Files which have been compiled, stored so that our special
    * classloader can get at them.
    */
  val newFileDict = mutable.Map.empty[String, Array[Byte]]
  def addClassFile(name: String, bytes: Array[Byte]) = {
    val tuple = Left(name) -> bytes.sum.hashCode().toLong
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
    Right(path) -> (if (exists(path))path.mtime.toMillis else 0)
  }

  private var classpathSignature0 = parentSignature
  def classpathSignature = classpathSignature0
  def classpathHash(wd: Path) = {
    Util.md5Hash(
      // Include the current working directory in the classpath hash, to make
      // sure different scripts cached
      Iterator(wd.toString.getBytes) ++
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

  def cloneClassLoader(parent: ClassLoader = null): SpecialClassLoader = {

    // FIXME Not tailrec

     val newParent =
       if (parent == null)
         getParent match {
           case s: SpecialClassLoader => s.cloneClassLoader()
           case p => p
         }
       else
         parent

     val clone = new SpecialClassLoader(newParent, parentSignature, getURLs.toSeq: _*)
     clone.newFileDict ++= newFileDict
     clone.classpathSignature0 = classpathSignature0

     clone
  }

}
