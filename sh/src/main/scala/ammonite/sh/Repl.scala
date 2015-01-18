package ammonite
package sh

import scala.tools.nsc.Settings

object Repl {
  def main(args: Array[String]): Unit = {
    val mainThread = Thread.currentThread()
    val settings = new Settings
    settings.embeddedDefaults[Repl.type]

    val loop = new CustomILoop

    import sun.misc.Signal
    import sun.misc.SignalHandler

    Signal.handle(new Signal("INT"), new SignalHandler () {
      def handle(sig: Signal) {
        loop.currentlyRunning match{
          case true =>
            mainThread.stop()
            loop.currentlyRunning = false
          case false => println("Ctrl-D again to exit")
        }
      }
    })

    loop.process(settings)

  }
}
