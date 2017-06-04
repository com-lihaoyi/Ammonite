package ammonite.interp

import ammonite.ops._
import ammonite.util.{Colors, Ref}
import ammonite.runtime.APIHolder
import ammonite.runtime.Evaluator.AmmoniteExit

import scala.collection.mutable


object InterpBridge extends APIHolder[InterpAPI]

trait InterpAPI {
  /**
    * When running a script in `--watch` mode, re-run the main script if this
    * file changes. By default, this happens for all script files, but you can
    * call this to watch arbitrary files your script may depend on
    */
  def watch(p: Path): Unit

  /**
    * The colors that will be used to render the Ammonite REPL in the terminal,
    * or for rendering miscellaneous info messages when running scripts.
    */
  val colors: Ref[Colors]

  /**
   * Tools related to loading external scripts and code into the REPL
   */
  def load: Load

  /**
   * resolvers to use when loading jars
   */
  def repositories: Ref[List[coursier.Repository]]

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
  /**
    * Configures the current compiler, or if the compiler hasn't been initialized
    * yet, registers the configuration callback and applies it to the compiler
    * when it ends up being initialized later
    */
  def configureCompiler(c: scala.tools.nsc.Global => Unit): Unit
}


trait LoadJar {

  /**
   * Load a `.jar` file or directory into your JVM classpath
   */
  def cp(jar: Path): Unit
  /**
   * Load one or more `.jar` files or directories into your JVM classpath
   */
  def cp(jars: Seq[Path]): Unit
  /**
   * Load a library from its maven/ivy coordinates
   */
  def ivy(coordinates: coursier.Dependency*): Unit
}

trait Load extends (String => Unit) with LoadJar{
  /**
   * Loads a command into the REPL and
   * evaluates them one after another
   */
  def apply(line: String): Unit

  /**
   * Loads and executes the scriptfile on the specified path.
   * Compilation units separated by `@\n` are evaluated sequentially.
   * If an error happens it prints an error message to the console.
   */
  def exec(path: Path): Unit

  def module(path: Path): Unit

  def plugin: LoadJar

}
