package ammonite

import java.io.{InputStream, OutputStream, PrintStream}

import ammonite.compiler.DefaultCodeWrapper
import ammonite.compiler.iface.CompilerBuilder
import ammonite.interp.PredefInitialization
import ammonite.interp.script.AmmoniteBuildServer
import ammonite.main._
import ammonite.util.Util.newLine
import ammonite.util._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

// needed to support deprecated Main.main
import acyclic.skipped

object AmmoniteMain {

  /**
   * The command-line entry point, which does all the argument parsing before
   * delegating to [[Main.run]]
   */
  def main(args0: Array[String]): Unit = {
    // set proxy properties from env
    // Not in `main0`, since `main0` should be able to be run as part of the
    // test suite without mangling the global properties of the JVM process
    ProxyFromEnv.setPropProxyFromEnv()

    val success = main0(args0.toList, System.in, System.out, System.err)
    if (success) sys.exit(0)
    else sys.exit(1)
  }

  /**
   * The logic of [[main]], in a form that doesn't call `sys.exit` and thus
   * can be unit tested without spinning up lots of separate, expensive
   * processes
   */
  def main0(
      args: List[String],
      stdIn: InputStream,
      stdOut: OutputStream,
      stdErr: OutputStream
  ): Boolean = {
    val printErr = new PrintStream(stdErr)
    val printOut = new PrintStream(stdOut)

    val customName = s"Ammonite REPL & Script-Runner, ${ammonite.Constants.version}"
    val customDoc = "usage: amm [ammonite-options] [script-file [script-options]]"
    Config.parser.constructEither(args, customName = customName, customDoc = customDoc) match {
      case Left(msg) =>
        printErr.println(msg)
        false
      case Right(cliConfig) =>
        if (cliConfig.core.bsp.value) {
          val buildServer = new AmmoniteBuildServer(
            ammonite.compiler.CompilerBuilder(),
            ammonite.compiler.Parsers,
            ammonite.compiler.DefaultCodeWrapper,
            initialScripts = cliConfig.rest.value.map(os.Path(_)),
            initialImports = PredefInitialization.initBridges(
              Seq("ammonite.interp.api.InterpBridge" -> "interp")
            ) ++ AmmoniteBuildServer.defaultImports
          )
          printErr.println("Starting BSP server")
          val (launcher, shutdownFuture) = AmmoniteBuildServer.start(buildServer)
          Await.result(shutdownFuture, Duration.Inf)
          printErr.println("BSP server done")
          true
        } else if (cliConfig.core.showVersion.value) {
          printOut.println(customName)
          true
        } else {

          val runner = new MainRunner(
            cliConfig,
            printOut,
            printErr,
            stdIn,
            stdOut,
            stdErr,
            os.pwd
          )

          if (cliConfig.repl.noRemoteLogging.value) {
            val msg = "Option --no-remote-logging is deprecated (remote logging has been removed)"
            runner.printInfo(msg)
          }

          (cliConfig.core.code, cliConfig.rest.value.toList) match {
            case (Some(code), Nil) =>
              runner.runCode(code)

            case (None, Nil) =>
              runner.printInfo("Loading...")
              runner.runRepl()
              true

            case (None, head :: rest) if head.startsWith("-") =>
              val failureMsg =
                "Unknown Ammonite option: " + head + Util.newLine +
                  "Use --help to list possible options"

              runner.printError(failureMsg)
              false

            case (None, head :: rest) =>
              val success = runner.runScript(os.Path(head, os.pwd), rest)
              success
          }
        }
    }
  }

}
