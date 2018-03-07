package ammonite.runtime

import java.io.File
import java.nio.file.Files
import java.util.zip.ZipFile

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

  private var _classpath: Vector[java.io.File] = _

  def classpath(storageBackend: Storage): Vector[java.io.File] = {
    if (_classpath != null) return _classpath
    var current = Thread.currentThread().getContextClassLoader
    val files = {
      val r = collection.mutable.Buffer.empty[java.io.File]
      val sunBoot = System.getProperty("sun.boot.class.path")
      if (sunBoot != null) {
        r.appendAll(
          sunBoot.split(java.io.File.pathSeparator)
            .map(new java.io.File(_))
        )
      } else {
        val rtPrefix = "rt-"
        val rtName = s"$rtPrefix${System.getProperty("java.version")}"
        val rt = storageBackend match {
          case storageBackend: Storage.Folder =>
            val rtFile = (storageBackend.dir / s"$rtName.jar").toIO
            for (f <- Option(rtFile.getParentFile.listFiles).getOrElse(Array())) {
              val fName = f.getName
              if (fName.startsWith(rtPrefix) && fName.endsWith(".jar")) {
                f.delete()
              }
            }
            rtFile
          case _: Storage.InMemory =>
            val f = Files.createTempFile(rtName, ".jar").toFile
            f.deleteOnExit()
            f.delete()
            f
        }
        r.append(rt)
        if (!rt.exists) {
          rt.getParentFile.mkdirs()
          Export.main(Array(rt.getCanonicalPath))
        }
        r.append(new File(getClass.getProtectionDomain.getCodeSource.getLocation.toURI))
      }
      while (current != null) {
        current match {
          case t: java.net.URLClassLoader =>
            r.appendAll(t.getURLs.map(u => new java.io.File(u.toURI)))
          case _ =>
        }
        current = current.getParent
      }
      r
    }
    _classpath = files.toVector.filter(_.exists)
    _classpath
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
