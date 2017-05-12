package ammonite.sshd

import java.io.{InputStream, OutputStream}

import org.apache.sshd.server._

/**
 * Implementation of ssh server's remote shell session,
 * which will be serving remote user.
 * @param remoteShell actual shell implementation,
 *                    which will serve remote user's shell session.
 */
private[sshd] class ShellSession(remoteShell: ShellSession.Server) extends Command {
  var in: InputStream = _
  var out: OutputStream = _
  var exit: ExitCallback = _
  lazy val thread = createShellServingThread()

  override def setInputStream(in: InputStream) = {
    this.in = in
  }
  override def setOutputStream(out: OutputStream) = {
    this.out = new SshOutputStream(out)
  }
  /* ammonite doesn't uses err stream so we don't need this */
  override def setErrorStream(err: OutputStream) = {}

  /**
   * called by ssh server to instrument this session
   * with a callback that it finished serving a user
   */
  override def setExitCallback(exit: ExitCallback): Unit = {
    this.exit = exit
  }

  /**
   * called when ssh server is ready to start this session.
   * Starts the actual shell-serving task.
   */
  override def start(env: Environment) = {
    thread.start()
  }

  /**
   * called when ssh server wants to destroy shell session.
   * Whatever shell session serving a user was doing at this moment
   * we are free to stop it.
   */
  override def destroy() = {
    thread.interrupt()
  }


  private def createShellServingThread(): Thread = new Thread {
    override def run(): Unit = {
      remoteShell(in, out)
      exit.onExit(0, "repl finished")
    }
  }


  // proxy which fixes output to the remote side to be ssh compatible.
  private class SshOutputStream(out: OutputStream) extends OutputStream {
    override def close() = { out.close() }
    override def flush() = { out.flush() }

    override def write(byte: Int): Unit = {
      // ssh client's only accepts new lines with \r so we make \n to be \r\n.
      // Unneeded \r will not be seen anyway
      if (byte.toChar == '\n') out.write('\r')
      out.write(byte)
    }

    override def write(bytes: Array[Byte]): Unit = for {
      i ← bytes.indices
    } write(bytes(i).toInt)

    override def write(bytes: Array[Byte], offset: Int, length: Int): Unit = {
      write(bytes.slice(offset, offset + length))
    }
  }
}

object ShellSession {
  type Server = ((InputStream, OutputStream) => Unit)
}
