import ammonite.ops._
val scalaVersion = scala.util.Properties.versionNumberString.split("\\.").dropRight(1).mkString(".")
val ammVersion = ammonite.Constants.version
load.cp(cwd/'shell/'target/s"scala-$scalaVersion"/s"ammonite-shell_$scalaVersion-$ammVersion.jar")
@
val shellSession = ammonite.shell.ShellSession()
import shellSession._
import ammonite.ops._
import ammonite.shell.PPrints._
import ammonite.shell._
ammonite.shell.Configure(repl, wd)
