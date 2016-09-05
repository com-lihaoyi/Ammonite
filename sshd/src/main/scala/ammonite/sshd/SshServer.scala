package ammonite.sshd

import java.util.Collections

import ammonite.ops.Path
import org.apache.sshd.agent.SshAgentFactory
import org.apache.sshd.common._
import org.apache.sshd.common.file.FileSystemFactory
import org.apache.sshd.common.session.ConnectionService
import org.apache.sshd.server.keyprovider.SimpleGeneratorHostKeyProvider
import org.apache.sshd.server.session.ServerSession
import org.apache.sshd.server.{Command, CommandFactory, PasswordAuthenticator}
import org.apache.sshd.{SshServer => SshServerImpl}

/**
  * A factory to simplify creation of ssh server
  */
object SshServer {
  def apply(options: SshServerConfig, shellServer: ShellSession.Server) = {
    val sshServer = SshServerImpl.setUpDefaultServer()
    sshServer.setHost(options.address)
    sshServer.setPort(options.port)
    sshServer.setPasswordAuthenticator(
      passwordAuthenticator(options.username, options.password)
    )
    sshServer.setKeyPairProvider(keyPairProvider(options))
    sshServer.setShellFactory(new Factory[Command] {
      override def create(): Command = new ShellSession(shellServer)
    })
    disableUnsupportedChannels(sshServer)
  }

  def keyPairProvider(options: SshServerConfig) = {
    val hostKeyFile = touch(
      options.hostKeyFile.getOrElse(fallbackHostkeyFilePath(options))
    )
    new SimpleGeneratorHostKeyProvider(hostKeyFile.toString())
  }

  private def disableUnsupportedChannels(sshServer: SshServerImpl) = {
    // exec can't really be disabled
    // but it can report error on trying to run any command it received
    sshServer.setCommandFactory(new CommandFactory {
      override def createCommand(command: String): Command =
        throw new IllegalArgumentException("exec is not supported")
    })
    sshServer.setSubsystemFactories(Collections.emptyList())
    sshServer.setTcpipForwardingFilter(null)
    sshServer.setAgentFactory(new SshAgentFactory {
      override def createServer(service: ConnectionService) = null
      override def createClient(manager: FactoryManager) = null
      override def getChannelForwardingFactory = null
    })
    sshServer.setFileSystemFactory(new FileSystemFactory {
      override def createFileSystemView(session: Session) = null
    })
    sshServer
  }

  // this is a user-safe options.
  // Server should have stable key
  // to not violate the user under threat of MITM attack
  private def fallbackHostkeyFilePath(options: SshServerConfig) =
    options.ammoniteHome / 'cache / 'ssh / 'hostkeys

  def touch(file: Path): Path = {
    import ammonite.ops._
    if (!exists(file)) {
      write(file, Array.empty[Byte])
    }
    file
  }

  private def passwordAuthenticator(correctUsername: String, correctPassword: String) =
    new PasswordAuthenticator {
      override def authenticate(username: String, password: String, session: ServerSession) =
        username == correctUsername && password == correctPassword
    }
}
