package ammonite.runtime

import java.io.File
import java.util.zip.ZipFile

import ammonite.ops.Path
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

  def rtJarName = s"rt-${System.getProperty("java.version")}.jar"

  /**
   * In memory cache of all the jars used in the compiler. This takes up some
   * memory but is better than reaching all over the filesystem every time we
   * want to do something.
   */
  def classpath(storage: Storage): Vector[File] = {
    val cache = storage.classpathCache()
    if (cache != null) return cache
    def rtCacheDir(storage: Storage): Option[Path] = storage match {
      case storage: Storage.Folder =>
        // no need to cache if the storage is in tmpdir because
        // Export.export creates the rt.jar in tmpdir
        if (storage.dir.toNIO.startsWith(
          java.nio.file.Paths.get(System.getProperty("java.io.tmpdir"))))
          None
        else Some(storage.dir)
      case _ => None
    }

    var current = Thread.currentThread().getContextClassLoader
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
            val tmpRt = Export.export()
            rtCacheDir(storage) match {
              case Some(path) =>
                val rt = (path / rtJarName).toIO
                if (!rt.exists)
                  java.nio.file.Files.copy(tmpRt.toPath, rt.toPath)
                files.append(rt)
              case _ => files.append(tmpRt)
            }
        }
      }
    }
    val r = files.toVector.filter(_.exists)
    storage.classpathCache.update(r)
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