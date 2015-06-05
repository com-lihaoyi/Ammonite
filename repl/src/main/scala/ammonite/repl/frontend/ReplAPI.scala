package ammonite.repl.frontend

import java.io.File

import ammonite.pprint.{PPrinter, PPrint, Config, TPrint}
import ammonite.repl.Ref

import scala.reflect.runtime.universe._
import acyclic.file

import scala.util.control.ControlThrowable


class ReplAPIHolder {
  var shell0: FullReplAPI = null
  lazy val shell = shell0
}

/**
 * Thrown to exit the REPL cleanly
 */
case object ReplExit extends ControlThrowable

trait ReplAPI {
  /**
   * Exit the Ammonite REPL. You can also use Ctrl-D to exit
   */
  def exit = throw ReplExit

  /**
   * Clears the screen of the REPL
   */
  def clear: Unit

  /**
   * Read/writable prompt for the shell. Use this to change the
   * REPL prompt at any time!
   */
  def shellPrompt: Ref[String]
  /**
   * The front-end REPL used to take user input. Modifiable!
   */
  def frontEnd: Ref[FrontEnd]

  /**
   * Display this help text
   */
  def help: String

  /**
   * History of commands that have been entered into the shell
   */
  def history: Seq[String]

  /**
   * Get the `Type` object of [[T]]. Useful for finding
   * what its methods are and what you can do with it
   */
  def typeOf[T: WeakTypeTag]: Type

  /**
   * Get the `Type` object representing the type of `t`. Useful
   * for finding what its methods are and what you can do with it
   *
   */
  def typeOf[T: WeakTypeTag](t: => T): Type
  
  /**
   * Tools related to loading external scripts and code into the REPL
   */
  def load: Load

  def colors: Ref[ColorSet]

  /**
   * Throw away the current scala.tools.nsc.Global and get a new one
   */
  def newCompiler(): Unit

  def search(target: scala.reflect.runtime.universe.Type): Option[String]

  def compiler: scala.tools.nsc.Global
  /**
   * Prettyprint the given `value` with no truncation. Optionally takes
   * a number of lines to print.
   */
  def show[T](value: T, lines: Int = 0): ammonite.pprint.Show[T]
  /**
   * Show all the imports that are used to execute commands going forward
   */
  def imports: String
  /**
   * Controls how things are pretty-printed in the REPL. Feel free
   * to shadow this with your own definition to change how things look
   */
  implicit var pprintConfig: ammonite.pprint.Config
}
trait Load extends (String => Unit){
  /**
   * Load a `.jar` file
   */
  def jar(jar: java.io.File): Unit
  /**
   * Load a library from its maven/ivy coordinates
   */
  def ivy(coordinates: (String, String, String), verbose: Boolean = true): Unit

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
  def script(path: String): Unit

  def script(file: File): Unit
}

// End of ReplAPI
/**
 * Things that are part of the ReplAPI that aren't really "public"
 */
abstract class FullReplAPI extends ReplAPI{
  val Internal: Internal
  trait Internal{
    def combinePrints(iters: Iterator[String]*): Iterator[String]
    def print[T: TPrint: PPrint: WeakTypeTag](value: => T, ident: String, custom: Option[String])(implicit cfg: Config): Iterator[String]
    def printDef(definitionLabel: String, ident: String): Iterator[String]
    def printImport(imported: String): Iterator[String]
  }
  def typeOf[T: WeakTypeTag] = scala.reflect.runtime.universe.weakTypeOf[T]
  def typeOf[T: WeakTypeTag](t: => T) = scala.reflect.runtime.universe.weakTypeOf[T]
}

object ReplAPI{
  def initReplBridge(holder: Class[ReplAPIHolder], api: ReplAPI) = {
    val method = holder
      .getDeclaredMethods
      .find(_.getName == "shell0_$eq")
      .get
    method.invoke(null, api)
  }
}
 
/**
 * A set of colors used to highlight the miscellanious bits of the REPL.
 */
case class ColorSet(prompt: String, ident: String, `type`: String, reset: String)
object ColorSet{
  val Default = ColorSet(Console.MAGENTA, Console.CYAN, Console.GREEN, Console.RESET)
  val BlackWhite = ColorSet("", "", "", "")
}

trait DefaultReplAPI extends FullReplAPI {

  def help = "Hello!"
  object Internal extends Internal{
    def combinePrints(iters: Iterator[String]*) = {
      iters.toIterator
           .filter(!_.isEmpty)
           .flatMap(Iterator("\n") ++ _)
           .drop(1)
    }
    def print[T: TPrint: PPrint: WeakTypeTag](value: => T, ident: String, custom: Option[String])(implicit cfg: Config) = {
      if (typeOf[T] =:= typeOf[Unit]) Iterator()
      else {
        val pprint = implicitly[PPrint[T]]
        val rhs = custom match {
          case None => pprint.render(value)
          case Some(s) => Iterator(pprint.cfg.color.literal(s))
        }
        Iterator(
          colors().ident, ident, colors().reset, ": ",
          implicitly[TPrint[T]].render(cfg), " = "
        ) ++ rhs
      }
    }
    def printDef(definitionLabel: String, ident: String) = {
      Iterator("defined ", colors().`type`, definitionLabel, " ", colors().ident, ident, colors().reset)
    }
    def printImport(imported: String) = {
      Iterator(colors().`type`, "import ", colors().ident, imported, colors().reset)
    }
  }
}
