// Basically a copy of example-predef.sc that's used for the test suite
import ammonite.ops._
import ammonite.interp.api.IvyConstructor.scalaBinaryVersion
val scalaVersion = scala.util.Properties.versionNumberString
val ammVersion = ammonite.Constants.version
interp.load.cp(Path(sys.env("AMMONITE_SHELL"), pwd))
@
val shellSession = ammonite.shell.ShellSession()
import shellSession._
import ammonite.ops._
import ammonite.shell._

// Doesn't work in the test suite, where it's run as a script rather than
// as a REPL

// ammonite.shell.Configure(interp, repl, wd)
