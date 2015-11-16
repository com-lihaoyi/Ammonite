import ammonite.ops._
load.jar(cwd/'shell/'target/"scala-2.11"/s"ammonite-shell_2.11-${ammonite.Constants.version}.jar")
@
val sess = ammonite.shell.ShellSession()
import sess._
import ammonite.ops._
import ammonite.shell.PPrints._
ammonite.shell.Configure(repl, wd)
