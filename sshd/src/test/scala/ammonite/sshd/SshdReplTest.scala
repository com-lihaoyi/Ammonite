package ammonite.sshd

import utest._
import utest.framework.TestSuite

import scala.concurrent.{Await, Promise}
import scala.language.postfixOps
import scala.concurrent.duration._
import SshTestingUtils._

object SshdReplTest extends TestSuite {
  val remotePromise = Promise[Boolean]()

  override val tests = TestSuite {
    'canExecuteRemoteCommand{
      withTmpDirectory { tmpDir =>
        val repl = new SshdRepl(SshServerConfig("localhost", 0, testUsername, testPassword, tmpDir))
        repl.start()
        val client = sshClient((testUsername, testPassword), "localhost", repl.port)
        client.connect()
        assert(client.isConnected)

        val shell = new Shell(client)
        shell.connect()
        assert(shell.isConnected)

        shell.input.println("2 + 2")
        shell.input.println("import ammonite.sshd.SshdReplTest")
        shell.input.println("SshdReplTest.remotePromise.success(true)")
        shell.input.println("exit")

        assert(Await.result(remotePromise.future, 60 seconds))
        shell.awaitToBecomeDisconnected(60 seconds)

        val outputLines = shell.output.readAll.lines.toList
        val Seq(firstBannerLine, secondBannerLine) = outputLines.take(2)
        val Seq(mathTestPrompt, mathOutput) = outputLines.slice(2, 4)
        val Seq(importAppPackage, importResult) = outputLines.slice(4, 6)

        assert(
          firstBannerLine.startsWith("Welcome to the Ammonite Repl"),
          secondBannerLine.contains("Scala"),
          secondBannerLine.contains("Java")
        )
        assert(
          mathTestPrompt.contains("2"),
          mathTestPrompt.contains("+"),
          mathOutput.contains("4")
        )
        assert(
          importAppPackage.contains("import"),
          importAppPackage.contains("ammonite.sshd.SshdReplTest"),
          !importResult.toLowerCase.contains("error")
        )
      }
    }
  }
}
