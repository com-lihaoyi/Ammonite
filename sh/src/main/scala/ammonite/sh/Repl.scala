package ammonite
package sh

import jline.console.ConsoleReader

import scala.tools.nsc.Settings

object Repl {
  def main(args: Array[String]): Unit = {
    new sh2.Shell2().run(args)

  }
}
