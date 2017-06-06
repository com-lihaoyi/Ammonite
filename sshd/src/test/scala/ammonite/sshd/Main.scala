package ammonite.sshd

import java.security.PublicKey
import org.apache.sshd.common.config.keys.{KeyUtils, PublicKeyEntryResolver}
import org.apache.sshd.server.auth.password.PasswordAuthenticator
import org.apache.sshd.server.auth.pubkey.PublickeyAuthenticator
import org.apache.sshd.server.config.keys.AuthorizedKeysAuthenticator
import org.apache.sshd.server.session.ServerSession
import scala.annotation.tailrec
import scala.collection.JavaConverters._

object Main {
  def main(args:Array[String]): Unit = {
    val config = SshServerConfig(
      "localhost", 2222,
      passwordAuthenticator = None,
      publicKeyAuthenticator = Some(publicKeyChecker)
    )
    val ammoniteServer = new SshdRepl(config)

    ammoniteServer.start()
    println(s"Ammonite server started. $helpMessage." +
            s"To connect use ssh [${currentUserName}@]<host> -p${config.port}")
    exitAwaitLoop()
    ammoniteServer.stop()

    println("Ammonite server finished")
  }

  object publicKeyChecker extends PublickeyAuthenticator {
    def authenticate(username: String, key: PublicKey, session: ServerSession): Boolean = {
      val keys = AuthorizedKeysAuthenticator.readDefaultAuthorizedKeys()
      username == currentUserName &&
        keys.asScala
          .exists { entry =>
            KeyUtils.compareKeys(entry.resolvePublicKey(PublicKeyEntryResolver.IGNORING), key)
          }
    }
  }

  object passwordChecker extends PasswordAuthenticator {
    def authenticate(username: String, password: String, session: ServerSession): Boolean = {
      username == currentUserName && password == getPassword
    }
  }

  def currentUserName = System.getProperty("user.name")

  private def getPassword = "password"

  private val helpMessage = "Print 'q' to quit."

  @tailrec private def exitAwaitLoop(): Unit = System.console().readLine() match {
    case "q" => println("exiting...")
    case cmd =>
      println(s"'$cmd' is illegal command! $helpMessage")
      exitAwaitLoop()
  }
}
