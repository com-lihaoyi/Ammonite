package ammonite.session

import ammonite.{DualTestRepl, TestUtils}
import utest._

import scala.collection.{immutable => imm}
object BuiltinTests extends TestSuite{

  val tests = Tests{
    println("BuiltinTests")
    val check = new DualTestRepl()
    test("basicConfig"){
      check.session("""
        @ // Set the shell prompt to be something else

        @ repl.prompt() = ">"

        @ // Change the terminal front end; the default is

        @ // Ammonite on Linux/OSX and JLineWindows on Windows

        @ repl.frontEnd() = frontEnd("unix")

        @ repl.frontEnd() = frontEnd("windows")

        @ repl.frontEnd() = frontEnd("ammonite")

        @ // Changing the colors used by Ammonite; all at once:

        @ interp.colors() = ammonite.util.Colors.BlackWhite

        @ interp.colors() = ammonite.util.Colors.Default

        @ // or one at a time:

        @ interp.colors().prompt() = fansi.Color.Red

        @ interp.colors().ident() = fansi.Color.Green

        @ interp.colors().`type`() = fansi.Color.Yellow

        @ interp.colors().literal() = fansi.Color.Magenta

        @ interp.colors().prefix() = fansi.Color.Cyan

        @ interp.colors().comment() = fansi.Color.Red

        @ interp.colors().keyword() = fansi.Bold.On

        @ interp.colors().selected() = fansi.Underlined.On

        @ interp.colors().error() = fansi.Color.Yellow
      """)
    }

    test("imports"){
      check.session("""
        @ assert(repl.imports.toString == ""); assert(repl.fullImports.toString != "")

        @ val definedValue = 123

        @ assert(repl.imports.toString.contains("definedValue"))

        @ assert(repl.fullImports.toString.contains("definedValue"))

        @ assert(!repl.imports.toString.contains("ammonite.runtime.tools"))

        @ assert(repl.fullImports.toString.contains("ammonite.runtime.tools"))
      """)
    }

    test("loadCP"){
      check.session("""
        @ import ammonite.ops._, ImplicitWd._

        @ val javaSrc = pwd/"amm"/"src"/"test"/"resources"/"loadable"/"hello"/"Hello.java"

        @ mkdir! pwd/"target"/"loadCP"/"hello"

        @ cp.over(javaSrc, pwd/"target"/"loadCP"/"hello"/"Hello.java")

        @ %javac "target"/"loadCP"/"hello"/"Hello.java"  //This line causes problems in windows

        @ import $cp.target.loadCP  //This line causes problems in windows

        @ hello.Hello.hello()
        res6: String = "Hello!"
      """)
    }
    test("settings"){
      val fruitlessTypeTestWarningMessageBlahBlahBlah =
        "fruitless type test: a value of type List[Int] cannot also be a List[Double]"

      // not sure why that one doesn't pass in 2.13
      // even disabling the noimports and imports settings instead of setting noimports to false
      // doesn't seem to reinstate imports
      def sv = scala.util.Properties.versionNumberString
      // In 2.12.13, I would have expected things like
      //   interp.configureCompiler(_.settings.Wconf.tryToSet(List("any:wv", "cat=unchecked:ws")))
      // to re-instate the expected warning below, to no avail :|
      if (TestUtils.scala2_12 && sv.stripPrefix("2.12.").toInt <= 12) check.session(s"""
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
    test("infoLogging"){
      check.session("""
        @ 1 + 1
        res0: Int = 2

        @ repl.compiler.settings.debug.value = true

        @ 1 + 1
        info: running phase parser on
      """)
    }


    test("saveLoad"){
      check.session(
        """
        @ val veryImportant = 1
        veryImportant: Int = 1

        @ repl.sess.save()

        @ val oopsDontWantThis = 2
        oopsDontWantThis: Int = 2

        @ // Let's try this new cool new library

        @ import $ivy.`com.lihaoyi::scalatags:0.7.0 compat`

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
    test("saveLoad2"){
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
    test("discardLoadCommandResult"){
      test - check.session("""
        @ repl.sess.save("foo")

        @ val a = repl.sess.load("foo")

        @ a
        error: not found: value a
      """)

      test - check.session("""
        @ val n = 2
        n: Int = 2

        @ repl.sess.save("foo")

        @ val n = repl.sess.load("foo") // should not mask the previous 'n'

        @ val n0 = n
        n0: Int = 2
      """)
    }
    test("firstFrameNotFrozen"){
      check.session("""
        @ 2
        res0: Int = 2

        @ res0 // we should be able to access the result of the previous command
        res1: Int = 2
      """)
    }
  }
}
