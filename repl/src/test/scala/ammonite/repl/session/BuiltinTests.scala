package ammonite.repl.session

import ammonite.repl.Checker
import utest._

import scala.collection.{immutable => imm}
object BuiltinTests extends TestSuite{

  val tests = TestSuite{
    println("EvaluatorTests")
    val check = new Checker()
    'basicConfig{
      check.session("""
        @ // Set the shell prompt to be something else

        @ repl.prompt() = ">"

        @ // Change the terminal front end; the default is

        @ // Ammonite on Linux/OSX and JLineWindows on Windows

        @ repl.frontEnd() = ammonite.repl.frontend.FrontEnd.JLineUnix

        @ repl.frontEnd() = ammonite.repl.frontend.FrontEnd.JLineWindows

        @ repl.frontEnd() = ammonite.repl.frontend.AmmoniteFrontEnd()

        @ // Changing the colors used by Ammonite; all at once:

        @ repl.colors() = ammonite.repl.Colors.BlackWhite

        @ repl.colors() = ammonite.repl.Colors.Default

        @ // or one at a time:

        @ repl.colors().prompt() = Console.RED

        @ repl.colors().ident() = Console.GREEN

        @ repl.colors().`type`() = Console.YELLOW

        @ repl.colors().literal() = Console.MAGENTA

        @ repl.colors().prefix() = Console.CYAN

        @ repl.colors().comment() = Console.RED

        @ repl.colors().keyword() = Console.BOLD

        @ repl.colors().selected() = Console.UNDERLINED

        @ repl.colors().error() = Console.YELLOW
      """)
    }

    'loadCP{
      check.session("""
        @ import ammonite.ops._

        @ import ammonite.ops.ImplicitWd._

        @ val scalaVersion = ("scala-"+scala.util.Properties.versionNumberString.dropRight(2))

        @ val dest = cwd/'repl/'target/scalaVersion/  "test-classes"/'loadable

        @ %javac dest/'hello/"Hello.java"

        @ load.cp(dest)

        @ hello.Hello.hello()
        res6: String = "Hello!"
      """)
    }
    'settings{
      check.session("""
        @ List(1, 2, 3) + "lol"
        res0: String = "List(1, 2, 3)lol"

        @ compiler.settings.noimports.value = true

        @ List(1, 2, 3) + "lol" // predef imports disappear
        error: not found: value List

        @ compiler.settings.noimports.value = false

        @ List(1, 2, 3) + "lol"
        res3: String = "List(1, 2, 3)lol"

        @ object X extends Dynamic
        error: extension of type scala.Dynamic needs to be enabled

        @ compiler.settings.language.tryToSet(List("dynamics"))

        @ object X extends Dynamic
        defined object X

        @ 1 + 1 // other things still work

      """)
    }
  }
}
