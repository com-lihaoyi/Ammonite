package ammonite.runtime

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

  /**
   * In memory cache of all the jars used in the compiler. This takes up some
   * memory but is better than reaching all over the filesystem every time we
   * want to do something.
   */

  private def getCPFromProperty(property: String = "sun.boot.class.path"): Vector[File] = {
    @annotation.tailrec
    def loop (paths: Array[String],cp: Vector[File]): Vector[java.io.File] =
      if (paths.length <= 0) cp
      else loop(paths.tail,cp ++ Vector(new java.io.File(paths.head)))

    loop(System.getProperty(property).split(java.io.File.pathSeparator),Vector())
  }

  private def getCPFromClassLoader(
    loader:ClassLoader = Thread.currentThread.getContextClassLoader): Vector[File] = {
    @annotation.tailrec
    def loop(cl:ClassLoader,
             cp:Vector[File],
             f:(Array[java.net.URL],Vector[File]) => Vector[File]): Vector[File] = {
      cl match {
        case null => cp
        case t: java.net.URLClassLoader => loop(cl.getParent, f(t.getURLs,cp ++ Vector()), f)
        case _ => loop(cl.getParent, cp, f)
      }
    }

    //Looping over an Array of URIs
    @annotation.tailrec
    def loop2(l2: Array[java.net.URL],cp2: Vector[File]): Vector[File] = {
      if (l2.length <= 0) cp2
      else loop2(l2.tail,cp2 ++ Vector(new java.io.File(l2.head.toURI)))
    }

    loop(loader, Vector(), loop2)
  }

  val classpath = (getCPFromProperty() ++ getCPFromClassLoader()).filter(_.exists())

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
