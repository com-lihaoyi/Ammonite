package ammonite.repl.frontend

import java.io.File

import scala.reflect.runtime.universe._
import acyclic.file
class ReplAPIHolder {
  var shell0: FullReplAPI = null
  lazy val shell = shell0
}

class ReplExit


trait ReplAPI {
  /**
   * Exit the Ammonite REPL. You can also use Ctrl-D to exit
   */
  def exit = new ReplExit

  /**
   * Read/writable prompt for the shell. Use this to change the
   * REPL prompt at any time!
   */
  var shellPrompt: String

  /**
   * Display this help text
   */
  def help: String

  /**
   * History of commands that have been entered into the shell
   */
  def history: Seq[String]

  /**
   * Get the `Type` object of [[T]]
   */
  def typeOf[T: WeakTypeTag]: Type

  /**
   * Get the `Type` object representing the type of `t`
   */
  def typeOf[T: WeakTypeTag](t: => T): Type
  
  /**
   * Tools related to loading external scripts and code into the REPL
   */
  def load: Load

  /**
   * Throw away the current scala.tools.nsc.Global and get a new one
   */
  def newCompiler(): Unit

  /**
   * Controls how things are pretty-printed in the REPL. Feel free
   * to shadow this with your own definition to change how things look
   */
  implicit def pprintConfig: ammonite.pprint.Config
}
trait Load{
  /**
   * Load a `.jar` file
   */
  def jar(jar: java.io.File): Unit
  /**
   * Load a library from its maven/ivy coordinates
   */
  def ivy(groupId: String, artifactId: String, version: String): Unit

  def lines(lines: Seq[String]): Unit
}

/**
 * Things that are part of the ReplAPI that aren't really "public"
 */
abstract class FullReplAPI extends ReplAPI{
  def shellPPrint[T: WeakTypeTag](value: => T, ident: String): String
  def shellPrintDef(definitionLabel: String, ident: String): String
  def typeOf[T: WeakTypeTag] = scala.reflect.runtime.universe.weakTypeOf[T]
  def typeOf[T: WeakTypeTag](t: => T) = scala.reflect.runtime.universe.weakTypeOf[T]
}

object ReplAPI{
  def initReplBridge(holder: Class[ReplAPIHolder], api: ReplAPI) = {
    holder
      .getDeclaredMethods
      .find(_.getName.contains('$'))
      .get
      .invoke(null, api)
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

class DefaultReplAPI(history0: => Seq[String],
                     load0: java.io.File => Unit,
                     loadLines0: Seq[String] => Unit,
                     loadIvy0: (String, String, String) => Unit,
                     newCompiler0: () => Unit,
                     colors: ColorSet,
                     val pprintConfig: ammonite.pprint.Config)
                     extends FullReplAPI {

  var shellPrompt: String = colors.prompt + "@" + colors.reset
  def help = "Hello!"
  def history: Seq[String] = history0
  def shellPPrint[T: WeakTypeTag](value: => T, ident: String) = {
    colors.ident + ident + colors.reset + ": " +
    colors.`type` + weakTypeOf[T].toString + colors.reset
  }
  def shellPrintDef(definitionLabel: String, ident: String) = {
    s"defined ${colors.`type`}$definitionLabel ${colors.ident}$ident${colors.reset}"
  }
  object load extends Load{
    def lines(lines: Seq[String]) = loadLines0(lines)
    def jar(jar: File): Unit = load0(jar)
    def ivy(groupId: String, artifactId: String, version: String): Unit =
      loadIvy0(groupId, artifactId, version)
  }

  def newCompiler(): Unit = newCompiler0()
}