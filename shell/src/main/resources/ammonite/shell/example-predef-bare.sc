import ammonite.ops._
import ammonite.runtime.tools.IvyThing.scalaBinaryVersion
val scalaVersion = scala.util.Properties.versionNumberString
val ammVersion = ammonite.Constants.version
interp.load.cp(pwd/'shell/'target/s"scala-$scalaBinaryVersion"/s"ammonite-shell_$scalaVersion-$ammVersion.jar")
@
val shellSession = ammonite.shell.ShellSession()
import shellSession._
import ammonite.ops._
import ammonite.shell._

// Doesn't work in the test suite, where it's run as a script rather than
// as a REPL

// ammonite.shell.Configure(repl, wd)
