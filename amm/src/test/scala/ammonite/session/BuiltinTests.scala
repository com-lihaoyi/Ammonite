package ammonite.session

import ammonite.TestRepl
import utest._

import scala.collection.{immutable => imm}
object BuiltinTests extends TestSuite{

  val tests = TestSuite{
    println("EvaluatorTests")
    val check = new TestRepl()
    'basicConfig{
      check.session("""
        @ // Set the shell prompt to be something else

        @ repl.prompt() = ">"

        @ // Change the terminal front end; the default is

        @ // Ammonite on Linux/OSX and JLineWindows on Windows

        @ repl.frontEnd() = ammonite.repl.FrontEnd.JLineUnix

        @ repl.frontEnd() = ammonite.repl.FrontEnd.JLineWindows

        @ repl.frontEnd() = ammonite.repl.AmmoniteFrontEnd()

        @ // Changing the colors used by Ammonite; all at once:

        @ repl.colors() = ammonite.util.Colors.BlackWhite

        @ repl.colors() = ammonite.util.Colors.Default

        @ // or one at a time:

        @ repl.colors().prompt() = fansi.Color.Red

        @ repl.colors().ident() = fansi.Color.Green

        @ repl.colors().`type`() = fansi.Color.Yellow

        @ repl.colors().literal() = fansi.Color.Magenta

        @ repl.colors().prefix() = fansi.Color.Cyan

        @ repl.colors().comment() = fansi.Color.Red

        @ repl.colors().keyword() = fansi.Bold.On

        @ repl.colors().selected() = fansi.Underlined.On

        @ repl.colors().error() = fansi.Color.Yellow
      """)
    }

    'loadCP{
      check.session("""
        @ import ammonite.ops._, ImplicitWd._

        @ val javaSrc = pwd/'amm/'src/'test/'resources/'loadable/'hello/"Hello.java"

        @ mkdir! pwd/'target/'loadCP/'hello

        @ cp.over(javaSrc, pwd/'target/'loadCP/'hello/"Hello.java")

        @ %javac 'target/'loadCP/'hello/"Hello.java"  //This line causes problems in windows

        @ import $cp.target.loadCP  //This line causes problems in windows

        @ hello.Hello.hello()
        res6: String = "Hello!"
      """)
    }
    'settings{
      val fruitlessTypeTestWarningMessageBlahBlahBlah =
        "fruitless type test: a value of type List[Int] cannot also be a List[Double]"

      check.session(s"""
        @ // Disabling default Scala imports

        @ List(1, 2, 3) + "lol"
        res0: String = "List(1, 2, 3)lol"

        @ interp.configureCompiler(_.settings.noimports.value = true)

        @ List(1, 2, 3) + "lol" // predef imports disappear
        error: not found: value List

        @ interp.configureCompiler(_.settings.noimports.value = false)

        @ List(1, 2, 3) + "lol"
        res3: String = "List(1, 2, 3)lol"

        @ // Disabling Scala language-import enforcement

        @ object X extends Dynamic
        error: extension of type scala.Dynamic needs to be enabled

        @ interp.configureCompiler(_.settings.language.tryToSet(List("dynamics")))

        @ object X extends Dynamic
        defined object X

        @ 1 + 1 // other things still work

        @ // Enabling warnings (which are disabled by default)

        @ List(1) match { case _: List[Double] => 2 }
        res7: Int = 2

        @ interp.configureCompiler(_.settings.nowarnings.value = false)

        @ List(1) match { case _: List[Double] => 2 }
        warning: $fruitlessTypeTestWarningMessageBlahBlahBlah

        @ // Note you can refer to `repl.compiler` when interactive in the REPL

        @ // But you should use `interp.configureCompiler` in your scripts/predef

        @ // because `repl.compiler` may be `null` if the script is cached.

        @ repl.compiler.settings.nowarnings.value
        res10: Boolean = false
      """)
    }
    'infoLogging{
      check.session("""
        @ 1 + 1
        res0: Int = 2

        @ repl.compiler.settings.debug.value = true

        @ 1 + 1
        info: running phase parser on
      """)
    }


    'saveLoad {
      check.session(
        """
        @ val veryImportant = 1
        veryImportant: Int = 1

        @ repl.sess.save()

        @ val oopsDontWantThis = 2
        oopsDontWantThis: Int = 2

        @ // Let's try this new cool new library

        @ import $ivy.`com.lihaoyi::scalatags:0.6.2`

        @ veryImportant
        res4: Int = 1

        @ oopsDontWantThis
        res5: Int = 2

        @ import scalatags.Text.all._

        @ div("Hello").render
        res7: String = "<div>Hello</div>"

        @ // Oh no, maybe we don't want scalatags!

        @ repl.sess.load()

        @ veryImportant
        res9: Int = 1

        @ oopsDontWantThis
        error: not found: value oopsDontWantThis

        @ import scalatags.Text.all._
        error: not found: value scalatags
        """)
    }
    'saveLoad2{
      check.session("""
        @ val (x, y) = (1, 2)
        x: Int = 1
        y: Int = 2

        @ repl.sess.save("xy initialized")

        @ val z = x + y
        z: Int = 3

        @ repl.sess.save("first z")

        @ repl.sess.load("xy initialized")

        @ val z = x - y
        z: Int = -1

        @ repl.sess.save("second z")

        @ z
        res7: Int = -1

        @ repl.sess.load("first z")

        @ z
        res9: Int = 3

        @ repl.sess.load("second z")

        @ z
        res11: Int = -1
                    """)
    }
  }
}
