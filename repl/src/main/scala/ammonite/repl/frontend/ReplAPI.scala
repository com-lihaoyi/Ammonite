package ammonite.repl.frontend

import scala.reflect.runtime.universe._
import acyclic.file
class ReplAPIHolder {
  var shell0: ReplAPI = null
  lazy val shell = shell0
}
class ReplExit

abstract class ReplAPI {
  def exit = new ReplExit

  /**
   * Read/writable prompt for the shell
   */
  var shellPrompt: String
  def help: String

  /**
   * History of commands that have been entered into the shell
   */
  def history: Seq[String]
  def shellPPrint[T: WeakTypeTag](value: => T, ident: String): String

  /**
   * Get the `Type` object of [[T]]
   */
  def typeOf[T: WeakTypeTag] = scala.reflect.runtime.universe.weakTypeOf[T]

  /**
   * Get the `Type` object representing the type of `t`
   */
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
class DefaultReplAPI(history0: => Seq[String]) extends ReplAPI {

  var shellPrompt: String = Console.MAGENTA + "scala>" + Console.RESET
  def help = "Hello!"
  def history: Seq[String] = history0
  def shellPPrint[T: WeakTypeTag](value: => T, ident: String) = {
    Console.CYAN + ident + Console.RESET + ": " +
    Console.GREEN + weakTypeOf[T].toString + Console.RESET
  }
}