package ammonite.repl.eval
import acyclic.file

/**
 * Loads the jars that make up the classpath of the scala-js-fiddle
 * compiler and re-shapes it into the correct structure to satisfy
 * scala-compile and scalajs-tools
 */
object Classpath {
  /**
   * In memory cache of all the jars used in the compiler. This takes up some
   * memory but is better than reaching all over the filesystem every time we
   * want to do something.
   */

  var current = getClass.getClassLoader
  val urls = collection.mutable.Buffer.empty[java.io.File]
  urls.appendAll(
    System.getProperty("sun.boot.class.path")
          .split(":")
          .map(new java.io.File(_))
  )
  while(current != null){
    current match{
      case t: java.net.URLClassLoader =>
        urls.appendAll(t.getURLs.map(u => new java.io.File(u.toURI)))
      case _ =>
    }
    current = current.getParent
  }

  val (jarDeps, dirDeps) = urls.filter(_.exists).partition(_.toString.endsWith(".jar"))
}