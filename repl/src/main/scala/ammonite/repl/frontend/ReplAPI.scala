package ammonite.repl.frontend

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
  def typeOf[T: WeakTypeTag]

  /**
   * Get the `Type` object representing the type of `t`
   */
  def typeOf[T: WeakTypeTag](t: => T)
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

class DefaultReplAPI(history0: => Seq[String]) extends FullReplAPI {

  var shellPrompt: String = Console.MAGENTA + "scala>" + Console.RESET
  def help = "Hello!"
  def history: Seq[String] = history0
  def shellPPrint[T: WeakTypeTag](value: => T, ident: String) = {
    Console.CYAN + ident + Console.RESET + ": " +
    Console.GREEN + weakTypeOf[T].toString + Console.RESET
  }
  def shellPrintDef(definitionLabel: String, ident: String) = {
    s"defined ${Console.GREEN}$definitionLabel ${Console.CYAN}$ident${Console.RESET}"
  }
}