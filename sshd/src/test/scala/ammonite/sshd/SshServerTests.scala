package ammonite.sshd

import com.jcraft.jsch.{JSchException, Session}
import org.scalacheck.Prop.forAll
import utest._

import scala.concurrent.duration._
import scala.concurrent.{Await, Promise}
import SshTestingUtils._

object SshServerTests extends TestSuite with ScalaCheckSupport {
  override val tests = Tests {
    test("canConnectAndAuthenticate") {
      withTmpDirectory { implicit tmpDir =>
        check {
          forAll(genCreds) { user =>
            withTestSshServer(user) { server =>
              val client = sshClient(user, server)
              client.connect()
              assert(client.isConnected)
            }
          }
        }
      }
    }
    test("cantConnectWithInvalidCredentials") {
      withTmpDirectory { implicit tmpDir =>
        check {
          forAll(genNonMatchingCredsPair) { credsPair =>
            val (serverCreds, clientCreds) = credsPair
            withTestSshServer(serverCreds) { server =>
              val client = sshClient(clientCreds, server)
              val exception = intercept[JSchException] {
                client.connect()
              }
              assert(!client.isConnected, exception.getMessage == "Auth fail")
            }
          }
        }
      }
    }
    test("thenConnectedExecutesShellTerminalTask") {
      withTmpDirectory { implicit tmpDir =>
        val remoteShellGetsExecuted = Promise[Unit]()
        def shellSession = () => remoteShellGetsExecuted.success((): Unit)
        withTestSshServer(testUser, shellSession) { server =>
          val client = sshClient(testUser, server)
          client.connect()
          val shell = new Shell(client)
          shell.connect()
          Await.result(remoteShellGetsExecuted.future, 5.seconds)
        }
      }
    }
    test("cantOpenWildChannel") {
      withTmpDirectory { implicit tmpDir =>
        withTestSshServer(testUser) { server =>
          val client = sshClient(testUser, server)
          client.connect()
          assert(client.isConnected)
          for (channel <- rejectedChannelTypes) cantConnectToChannel(client, channel)
          check(forAll { (randomChannel: String) =>
            cantConnectToChannel(client, randomChannel)
          })
        }
      }
    }
  }

  private lazy val rejectedChannelTypes = Seq(
    "exec",
    "env",
    "x11-req",
    "x11",
    "subsystem",
    "exit-signal",
    "auth-agent-req@openssh.com"
  )

  private def cantConnectToChannel(client: Session, channel: String)(implicit
      dir: os.Path
  ): Unit = {
    assert(client.isConnected)
    Option(client.openChannel(channel)) match {
      case Some(shell) =>
        intercept[JSchException](shell.connect(2000))
        assert(!shell.isConnected)
      case None =>
    }
  }
}
