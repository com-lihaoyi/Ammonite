load.ivy("com.lihaoyi" %% "ammonite-shell" % ammonite.Constants.version)
@
val sess = ammonite.shell.ShellSession()
import sess._
import ammonite.shell.PPrints._
import ammonite.ops._
ammonite.shell.Configure(repl, wd)
