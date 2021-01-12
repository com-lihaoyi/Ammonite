package ammonite.interp.api


import ammonite.util.{Colors, Ref}
import coursierapi.{Dependency, Fetch, Repository}

import scala.collection.mutable


object InterpBridge extends APIHolder[InterpAPI]

trait InterpAPI {
  /**
    * When running a script in `--watch` mode, re-run the main script if this
    * file changes. By default, this happens for all script files, but you can
    * call this to watch arbitrary files your script may depend on
    */
  def watch(p: os.Path): Unit

  /**
   * A generalization of [[watch]], allows watching arbitrary values and not
   * just the contents of file paths.
   */
  def watchValue[T](v: => T): T

  /**
    * The colors that will be used to render the Ammonite REPL in the terminal,
    * or for rendering miscellaneous info messages when running scripts.
    */
  val colors: Ref[Colors]

  /**
   * Tools related to loading external scripts and code into the REPL
   */
  def load: InterpLoad

  /**
   * resolvers to use when loading jars
   */
  def repositories: Ref[List[Repository]]

  /**
    * Functions that will be chained and called on the coursier
    * Fetch object right before they are run
    */
  val resolutionHooks: mutable.Buffer[
    Fetch => Fetch
  ]

  /**
    * Exit the Ammonite REPL. You can also use Ctrl-D to exit
    */
  def exit = throw AmmoniteExit(())
  /**
    * Exit the Ammonite REPL. You can also use Ctrl-D to exit
    */
  def exit(value: Any) = throw AmmoniteExit(value)
  /**
    * Functions that will be chained and called on the
    * exitValue before the repl exits
    */
  val beforeExitHooks: mutable.Buffer[Any => Any]

  def _compilerManager: ammonite.compiler.iface.CompilerLifecycleManager
}


trait LoadJar {

  /**
   * Load a `.jar` file or directory into your JVM classpath
   */
  def cp(jar: os.Path): Unit
  /**
    * Load a `.jar` from a URL into your JVM classpath
    */
  def cp(jar: java.net.URL): Unit
  /**
   * Load one or more `.jar` files or directories into your JVM classpath
   */
  def cp(jars: Seq[os.Path]): Unit
  /**
   * Load a library from its maven/ivy coordinates
   */
  def ivy(coordinates: Dependency*): Unit
}

trait InterpLoad extends LoadJar{

  def module(path: os.Path): Unit

  def plugin: LoadJar

}
