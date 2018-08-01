package ammonite.runtime

import java.io.File
import java.util.zip.ZipFile

import ammonite.ops._
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
  def classpath(classLoader: ClassLoader, storage: Storage): Vector[File] = {
    val cache = storage.classpathCache()
    if (cache.isDefined) return cache.get
    def rtCacheDir(storage: Storage): Option[Path] = storage match {
      case storage: Storage.Folder =>
        // no need to cache if the storage is in tmpdir
        // because it is temporary
        if (storage.dir.toNIO.startsWith(
          java.nio.file.Paths.get(System.getProperty("java.io.tmpdir"))))
          None
        else Some(storage.dir)
      case _ => None
    }

    var current = classLoader
    val files = collection.mutable.Buffer.empty[java.io.File]
    while(current != null){
      current match{
        case t: java.net.URLClassLoader =>
          files.appendAll(t.getURLs.map(u => new java.io.File(u.toURI)))
        case _ =>
      }
      current = current.getParent
    }

    {
      val sunBoot = System.getProperty("sun.boot.class.path")
      if (sunBoot != null) {
        files.appendAll(
          sunBoot
            .split(java.io.File.pathSeparator)
            .map(new java.io.File(_))
        )
      } else {
        for (p <- System.getProperty("java.class.path")
          .split(File.pathSeparatorChar) if !p.endsWith("sbt-launch.jar")) {
          files.append(new File(p))
        }
        try {
          new java.net.URLClassLoader(files.map(_.toURI.toURL).toArray, null)
            .loadClass("javax.script.ScriptEngineManager")
        } catch {
          case _: ClassNotFoundException =>
            rtCacheDir(storage) match {
              case Some(path) => files.append(Export.rtAt(path.toIO))
              case _ => files.append(Export.rt())
            }
        }
      }
    }
    val r = files.toVector.filter(_.exists)
    storage.classpathCache.update(Some(r))
    r
  }

  def canBeOpenedAsJar(file: File): Boolean =
    try {
      val zf = new ZipFile(file)
      zf.close()
      true
    } catch {
      case NonFatal(e) =>
        traceClasspathProblem(
          s"Classpath element '${file.getAbsolutePath}' "+
            "could not be opened as jar file because of $e"
        )
        false
    }
  def traceClasspathProblem(msg: String): Unit =
    if (traceClasspathIssues) println(msg)
}