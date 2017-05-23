import ammonite.ops._
val scalaVersion = scala.util.Properties.versionNumberString.split("\\.").dropRight(1).mkString(".")
val ammVersion = ammonite.Constants.version
interp.load.cp(pwd/'shell/'target/s"scala-$scalaVersion"/s"ammonite-shell_$scalaVersion-$ammVersion.jar")
@
val shellSession = ammonite.shell.ShellSession()
import shellSession._
import ammonite.ops._
import ammonite.shell._
ammonite.shell.Configure(repl, wd)
