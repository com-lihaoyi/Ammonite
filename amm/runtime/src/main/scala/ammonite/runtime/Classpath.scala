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

  def classpath(storageBackend: Storage): Vector[java.io.File] = {
    val p = storageBackend.classpathRtJar()
    if (p._1.nonEmpty) return p._1 ++ p._2
    val sunBoot = System.getProperty("sun.boot.class.path")
    val rtOpt: Option[File] = if (sunBoot != null) None else {
      val rtPrefix = "rt-"
      val rtName = s"$rtPrefix${System.getProperty("java.version")}"
      val rt = storageBackend match {
        case storageBackend: Storage.Folder =>
          val rtFile = (storageBackend.dir / s"$rtName.jar").toIO
          val files = Option(rtFile.getParentFile.listFiles)
            .getOrElse(Array())
          for (f <- files) {
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
      if (!rt.exists) {
        rt.getParentFile.mkdirs()
        Export.main(Array(rt.getCanonicalPath))
      }
      Some(rt)
    }
    val files = {
      val r = collection.mutable.Buffer.empty[java.io.File]
      if (sunBoot != null) {
        r.appendAll(
          sunBoot.split(java.io.File.pathSeparator)
            .map(new java.io.File(_))
        )
      } else {
        val cp = new File(getClass
          .getProtectionDomain.getCodeSource.getLocation.toURI)
        if (cp.isDirectory) {
          for (p <- System.getProperty("java.class.path")
            .split(File.pathSeparatorChar) if !p.endsWith("sbt-launch.jar")) {
            r.append(new File(p))
          }
        } else {
          r.append(cp)
        }
      }
      var current = Thread.currentThread().getContextClassLoader
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
    val r = files.toVector.filter(_.exists)
    storageBackend.classpathRtJar.update((r, rtOpt))
    r ++ rtOpt
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
