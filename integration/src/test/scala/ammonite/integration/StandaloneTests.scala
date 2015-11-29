package ammonite.integration

import utest._
import ammonite.ops._
import ammonite.ops.ImplicitWd._
import utest.framework.TestSuite

/**
 * Run a small number of scripts using the Ammonite standalone executable,
 * to make sure that this works. Otherwise it tends to break since the
 * standalone executable has a pretty different classloading environment
 * from the "run in SBT on raw class files" that the rest of the tests use.
 *
 * These are also the only tests that cover all the argument-parsing
 * and configuration logic inside, which the unit tests don't cover since
 * they call the REPL programmatically
 */
object StandaloneTests extends TestSuite{
  // Prepare standalone executable
  val scalaVersion = scala.util.Properties.versionNumberString
  println("StandaloneTests")
  val tests = TestSuite {
    val ammVersion = ammonite.Constants.version
    val executableName = s"ammonite-repl-$ammVersion-$scalaVersion"
    val Seq(executable) = ls.rec! cwd |? (_.last == executableName)
    val replStandaloneResources = cwd/'integration/'src/'test/'resources/'ammonite/'integration
    val shellAmmoniteResources = cwd/'shell/'src/'main/'resources/'ammonite/'shell
    //use Symbol to wrap symbols with dashes.
    val emptyPrefdef = shellAmmoniteResources/"empty-predef.scala"
    val exampleBarePredef = shellAmmoniteResources/"example-predef-bare.scala"

    //we use an empty predef file here to isolate the tests from external forces.
    def exec(name: String, args: String*) = (
      %%bash(
        executable,
        "--predef-file",
        emptyPrefdef,
        replStandaloneResources/name,
        args
      )
    ).mkString("\n")

    'hello{
      val evaled = exec("Hello.scala")
      assert(evaled == "Hello World")
    }

    'complex{
      val evaled = exec("Complex.scala")
      assert(evaled.contains("Spire Interval [0, 10]"))
    }

    'shell{
      // make sure you can load the example-predef.scala, have it pull stuff in
      // from ivy, and make use of `cd!` and `wd` inside the executed script.
      val res = %%bash(
        executable,
        "--predef-file",
        exampleBarePredef,
        "-c",
        """val x = wd
          |@
          |cd! 'repl/'src
          |@
          |println(wd relativeTo x)""".stripMargin
      )

      val output = res.mkString("\n")
      assert(output == "repl/src")
    }
    'main{
      val evaled = exec("Main.scala")
      assert(evaled.contains("Hello! 1"))
    }
    'args{
      'full{
        val evaled = exec("Args.scala", "3", "Moo", (cwd/'omg/'moo).toString)
        assert(evaled.contains("Hello! MooMooMoo omg/moo"))
      }
      'default{
        val evaled = exec("Args.scala", "3", "Moo")
        assert(evaled.contains("Hello! MooMooMoo "))
      }
    }
  }
}
