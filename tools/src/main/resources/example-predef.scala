//load.ivy("com.lihaoyi" %% "ammonite-tools" % ammonite.Constants.version)
@
val sess = ammonite.tools.ShellSession()
import sess._
import ammonite.ops._
ammonite.tools.Configure(repl, wd)
