package ammonite.sshd

import scala.annotation.tailrec

object Main {
  def main(args:Array[String]): Unit = {
    val config = SshServerConfig("localhost", 2222, currentUserName, getPassword)
    val ammoniteServer = new SshdRepl(config)

    ammoniteServer.start()
    println(s"Ammonite server started. $helpMessage." +
            s"To connect use ssh [${config.username}@]<host> -p${config.port}")
    exitAwaitLoop()
    ammoniteServer.stop()

    println("Ammonite server finished")
  }

  def currentUserName = System.getProperty("user.name")
  
  private def getPassword: String = {
    System.out.print("Enter password for remote session: ")
    new String(System.console().readPassword())
  }

  private val helpMessage = "Print 'q' to quit."

  @tailrec private def exitAwaitLoop(): Unit = System.console().readLine() match {
    case "q" => println("exiting...")
    case cmd =>
      println(s"'$cmd' is illegal command! $helpMessage")
      exitAwaitLoop()
  }
}
