load.ivy("com.lihaoyi" %% "ammonite-shell" % ammonite.Constants.version)
@
val shellSession = ammonite.shell.ShellSession()
import shellSession._
import ammonite.shell.PPrints._
import ammonite.ops._
import ammonite.shell.time
ammonite.shell.Configure(repl, wd)
