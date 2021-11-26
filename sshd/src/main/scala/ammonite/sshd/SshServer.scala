package ammonite.sshd

import java.util.Collections

import org.apache.sshd.agent.SshAgentFactory
import org.apache.sshd.common._
import org.apache.sshd.common.file.FileSystemFactory
import org.apache.sshd.common.session.{ConnectionService, Session}
import org.apache.sshd.server.auth.keyboard.DefaultKeyboardInteractiveAuthenticator
import org.apache.sshd.server.keyprovider.SimpleGeneratorHostKeyProvider
import org.apache.sshd.server.{Command, CommandFactory, SshServer => SshServerImpl}

/**
 * A factory to simplify creation of ssh server
 */
object SshServer {
  def apply(options: SshServerConfig, shellServer: ShellSession.Server) = {
    val sshServer = SshServerImpl.setUpDefaultServer()
    sshServer.setHost(options.address)
    sshServer.setPort(options.port)
    options.passwordAuthenticator.foreach { auth =>
      sshServer.setPasswordAuthenticator(auth)
      sshServer.setKeyboardInteractiveAuthenticator(
        new DefaultKeyboardInteractiveAuthenticator()
      )
    }
    options.publicKeyAuthenticator.foreach { auth =>
      sshServer.setPublickeyAuthenticator(auth)
    }
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
    val provider = new SimpleGeneratorHostKeyProvider(hostKeyFile.wrapped)
    provider.setAlgorithm("RSA")
    provider
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
      override def createFileSystem(session: Session) = null
    })
    sshServer
  }

  // this is a user-safe options.
  // Server should have stable key
  // to not violate the user under threat of MITM attack
  private def fallbackHostkeyFilePath(options:SshServerConfig) =
    options.ammoniteHome/'cache/'ssh/'hostkeys

  def touch(file: os.Path): os.Path = {
    if (!os.exists(file)) {
      os.write(file, Array.empty[Byte], createFolders = true)
    }
    file
  }
}
