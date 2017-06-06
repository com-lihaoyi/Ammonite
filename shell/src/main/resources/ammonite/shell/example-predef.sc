interp.load.ivy(
  "com.lihaoyi" %
  s"ammonite-shell_${scala.util.Properties.versionNumberString}" %
  ammonite.Constants.version
)
@
val shellSession = ammonite.shell.ShellSession()
import shellSession._
import ammonite.ops._
import ammonite.shell._
ammonite.shell.Configure(interp, repl, wd)
