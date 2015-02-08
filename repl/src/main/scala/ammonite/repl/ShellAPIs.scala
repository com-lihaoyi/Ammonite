package ammonite.repl
import scala.reflect.runtime.universe._

class ShellAPIHolder {
  var shell0: ShellAPIs = null
  lazy val shell = shell0
}
class ShellExit

abstract class ShellAPIs {
  def exit = new ShellExit

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
