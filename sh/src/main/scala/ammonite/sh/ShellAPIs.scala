package ammonite.sh
import scala.reflect.runtime.universe._

class ShellAPIHolder {
  var shell: ShellAPIs = null
}
abstract class ShellAPIs {
  def exit: Unit
  def help: String
  def history: Seq[String]
  def shellPPrint[T: WeakTypeTag](value: => T, ident: String): String
}
