package ammonite.runtime

import ammonite.ops._
import ammonite.runtime.tools.Resolver
import ammonite.util.Ref
import acyclic.file
import scala.util.control.ControlThrowable


object InterpBridge extends APIHolder[InterpAPI]

/**
 * Thrown to exit the REPL cleanly
 */
case class ReplExit(value: Any) extends ControlThrowable


trait InterpAPI {

  /**
   * Tools related to loading external scripts and code into the REPL
   */
  def load: Load

  /**
   * resolvers to use when loading jars
   */
  def resolvers: Ref[List[Resolver]]

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
  def maven(coordinates: (String, String, String), verbose: Boolean = true): Unit

  def ivy(coordinates: (String, String, String), verbose: Boolean = true): Unit
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
