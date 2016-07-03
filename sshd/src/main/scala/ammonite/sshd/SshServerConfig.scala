package ammonite.sshd

import acyclic.file
import ammonite.main.Defaults
import ammonite.ops.Path

/**
 * Ssh server parameters
 * @param port a port to be used by ssh server. Set it as `0` to let server choose some random port.
 * @param username username to authenticate on ssh server
 * @param password password to authenticate on ssh server
 * @param ammoniteHome path that ammonite repl sessions will be using as their home directory
 * @param hostKeyFile path to the place where to store server's identity key
 */
case class SshServerConfig(address: String,
                           port: Int,
                           username:String,
                           password:String,
                           ammoniteHome: Path = Defaults.ammoniteHome,
                           hostKeyFile: Option[Path] = None
) {
  require(username.nonEmpty, "username can't be an empty string")
  override def toString =
    s"(port = $port, username = '$username'," +
     s"home = '$ammoniteHome', hostKeyFile = $hostKeyFile)"
}
