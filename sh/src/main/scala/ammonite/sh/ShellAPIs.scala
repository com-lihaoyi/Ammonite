package ammonite.sh


class ShellAPIHolder extends ShellAPIs{
  var intp: ShellAPIs = null
  def exit = intp.exit
  def help = intp.help
  def history = intp.history
}
abstract class ShellAPIs {
  def exit: Unit
  def help: String
  def history: Seq[String]
}
