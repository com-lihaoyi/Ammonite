package ammonite.repl

import utest._

import scala.collection.{immutable => imm}

object ConfigurationTests extends TestSuite{

  val tests = TestSuite{
    println("EvaluatorTests")
    val check = new Checker()
    'basicConfig{
      check.session("""
        @ // Set the shell prompt to be something else

        @ shellPrompt() = ">"

        @ // Change the terminal front end; the default is

        @ // Ammonite on Linux/OSX and JLineWindows on Windows

        @ frontEnd() = ammonite.repl.frontend.FrontEnd.JLineUnix

        @ frontEnd() = ammonite.repl.frontend.FrontEnd.JLineWindows

        @ frontEnd() = ammonite.repl.frontend.FrontEnd.Ammonite

        @ // Changing the colors used by Ammonite; all at once:

        @ colors() = ammonite.repl.ColorSet.BlackWhite

        @ colors() = ammonite.repl.ColorSet.Default

        @ // or one at a time:

        @ colors().prompt() = Console.RED

        @ colors().ident() = Console.GREEN

        @ colors().`type`() = Console.YELLOW

        @ colors().literal() = Console.MAGENTA

        @ colors().prefix() = Console.CYAN

        @ colors().comment() = Console.RED

        @ colors().keyword() = Console.BOLD

        @ colors().selected() = Console.UNDERLINED

        @ colors().error() = Console.YELLOW
      """)

    }
  }
}
