package ammonite.interp

import java.io.File
import java.util.zip.ZipFile

import acyclic.file

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

  def classpath(loader: ClassLoader) = {

    var current = loader

    val files = collection.mutable.Buffer.empty[java.io.File]
    files.appendAll(
      System.getProperty("sun.boot.class.path")
        .split(java.io.File.pathSeparator)
        .map(new java.io.File(_))
    )
    while(current != null){
      current match{
        case t: java.net.URLClassLoader =>
          files.appendAll(t.getURLs.map(u => new java.io.File(u.toURI)))
        case _ =>
      }
      current = current.getParent
    }

    files.toVector.filter(_.exists)
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