package ammonite.repl

object TestUtils {
  val sessionPrefix = if (scala2_10) "ammonite.session." else ""
  def scala2_10 = scala.util.Properties.versionNumberString.contains("2.10")
}
