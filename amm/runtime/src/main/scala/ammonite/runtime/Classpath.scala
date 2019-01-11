package ammonite.runtime

import java.io.File
import java.net.URL
import java.util.zip.{ZipFile, ZipInputStream}


import io.github.retronym.java9rtexport.Export

import scala.util.control.NonFatal

/**
 * Loads the jars that make up the classpath of the scala-js-fiddle
 * compiler and re-shapes it into the correct structure to satisfy
 * scala-compile and scalajs-tools
 */
object Classpath {
  val traceClasspathIssues =
    sys.props
       .get("ammonite.trace-classpath")
       .exists(_.toLowerCase == "true")

  /**
   * In memory cache of all the jars used in the compiler. This takes up some
   * memory but is better than reaching all over the filesystem every time we
   * want to do something.
   */
  def classpath(classLoader: ClassLoader, storage: Storage): Vector[URL] = {
    val cache = storage.classpathCache()
    if (cache.isDefined) return cache.get
    def rtCacheDir(storage: Storage): Option[os.Path] = storage match {
      case storage: Storage.Folder =>
        // no need to cache if the storage is in tmpdir
        // because it is temporary
        if (storage.dir.wrapped.startsWith(
          java.nio.file.Paths.get(System.getProperty("java.io.tmpdir"))))
          None
        else Some(storage.dir)
      case _ => None
    }

    var current = classLoader
    val files = collection.mutable.Buffer.empty[java.net.URL]
    val seenClassLoaders = collection.mutable.Buffer.empty[ClassLoader]
    while(current != null){
      seenClassLoaders.append(current)
      current match{
        case t: java.net.URLClassLoader =>
          files.appendAll(t.getURLs)
        case _ =>
      }
      current = current.getParent
    }


    val sunBoot = System.getProperty("sun.boot.class.path")
    if (sunBoot != null) {
      files.appendAll(
        sunBoot
          .split(java.io.File.pathSeparator)
          .map(new java.io.File(_))
          .filter(_.exists())
          .map(_.toURI.toURL)
      )
    } else {
      if (seenClassLoaders.contains(ClassLoader.getSystemClassLoader)) {
        for (p <- System.getProperty("java.class.path")
          .split(File.pathSeparatorChar) if !p.endsWith("sbt-launch.jar")) {
          val f = new File(p)
          if (f.exists())
            files.append(f.toURI.toURL)
        }
        try {
          new java.net.URLClassLoader(files.map(_.toURI.toURL).toArray, null)
            .loadClass("javax.script.ScriptEngineManager")
        } catch {
          case _: ClassNotFoundException =>
            rtCacheDir(storage) match {
              case Some(path) => files.append(Export.rtAt(path.toIO).toURI.toURL)
              case _ => files.append(Export.rt().toURI.toURL)
            }
        }
      }
    }
    val r = files.toVector
    storage.classpathCache.update(Some(r))
    r
  }

  def canBeOpenedAsJar(url: URL): Boolean = {
    var zis: ZipInputStream = null
    try {
      if (url.getProtocol == "file") {
        // this ignores any preamble in particular, unlike ZipInputStream
        val zf = new ZipFile(new File(url.toURI))
        zf.close()
        true
      } else {
        zis = new ZipInputStream(url.openStream())
        zis.getNextEntry != null
      }
    } catch {
      case NonFatal(e) =>
        traceClasspathProblem(
          s"Classpath element '$url' "+
            s"could not be opened as jar file because of $e"
        )
        false
    } finally {
      if (zis != null)
        zis.close()
    }
  }
  def traceClasspathProblem(msg: String): Unit =
    if (traceClasspathIssues) println(msg)
}
