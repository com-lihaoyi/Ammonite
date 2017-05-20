package ammonite.sshd


import ammonite.main.Defaults
import ammonite.ops.Path
import org.apache.sshd.server.auth.password.PasswordAuthenticator
import org.apache.sshd.server.auth.pubkey.PublickeyAuthenticator

/**
 * Ssh server parameters
 * @param port a port to be used by ssh server. Set it as `0` to let server choose some random port.
 * @param ammoniteHome path that ammonite repl sessions will be using as their home directory
 * @param hostKeyFile path to the place where to store server's identity key
 */
case class SshServerConfig(address: String,
                           port: Int,
                           ammoniteHome: Path = Defaults.ammoniteHome,
                           hostKeyFile: Option[Path] = None,
                           passwordAuthenticator: Option[PasswordAuthenticator] = None,
                           publicKeyAuthenticator: Option[PublickeyAuthenticator] = None
) {
  require(passwordAuthenticator.orElse(publicKeyAuthenticator).isDefined,
    "you must provide at least one authenticator")
  override def toString =
    s"(port = $port," +
      s"home = '$ammoniteHome', hostKeyFile = $hostKeyFile," +
      s"passwordAuthenticator = $passwordAuthenticator," +
      s"publicKeyAuthenticator = $publicKeyAuthenticator)"
}
