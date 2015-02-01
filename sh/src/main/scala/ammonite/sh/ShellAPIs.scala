package ammonite.sh
import scala.reflect.runtime.universe._

class ShellAPIHolder {
  var shell0: ShellAPIs = null
  lazy val shell = shell0
}
class ShellExit

abstract class ShellAPIs {
  def exit = new ShellExit
  var shellPrompt: String
  def help: String
  def history: Seq[String]
  def shellPPrint[T: WeakTypeTag](value: => T, ident: String): String
}
