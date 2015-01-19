package ammonite
package sh

import scala.tools.nsc.Settings

object Repl {
  def main(args: Array[String]): Unit = {
    val settings = new Settings
    settings.embeddedDefaults[Repl.type]
    val loop = new CustomILoop
    loop.process(settings)
  }
}
