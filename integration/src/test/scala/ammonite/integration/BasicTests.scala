package ammonite.integration

import utest._
import ammonite.ops._
import ammonite.ops.ImplicitWd._
import ammonite.util.Util
import TestUtils._
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
object BasicTests extends TestSuite{
  override def utestTruncateLength = 60000
  println("StandaloneTests")
  val tests = TestSuite {

    'hello{
      val evaled = exec('basic/"Hello.sc")
      assert(evaled.out.trim == "Hello World")
    }



    //These tests currently do not pass on Windows, primarily for the reason that they all
    //involve loading from ivy which has different settings for windows
    //http://stackoverflow.com/questions/15487301/configure-apache-ant-and-ivy-on-windows-7
    'linuxOnlyTests {

      'complex {
        if (!Util.windowsPlatform) {
          val evaled = exec('basic / "Complex.sc")
          assert(evaled.out.trim.contains("Spire Interval [0, 10]"))
        }
      }


      'shell {
        // make sure you can load the example-predef.sc, have it pull stuff in
        // from ivy, and make use of `cd!` and `wd` inside the executed script.
        if (!Util.windowsPlatform) {
          val res = %% bash(
            executable,
            "--predef-file",
            exampleBarePredef,
            "-c",
            """val x = wd
            |@
            |cd! 'amm/'src
            |@
            |println(wd relativeTo x)""".stripMargin
          )

          val output = res.out.trim
          assert(output == "amm/src")
        }
      }

      'classloaders{
        if (!Util.windowsPlatform) {
          val evaled = exec('basic / "Resources.sc")
          assert(evaled.out.string.contains("1745"))
        }
      }

      'playframework- {
        if (!Util.windowsPlatform) {
          if (scalaVersion.startsWith("2.11.") && javaVersion.startsWith("1.8.")){
            val evaled = exec('basic/"PlayFramework.sc")
            assert(evaled.out.string.contains("Hello bar"))
          }
        }
      }
    }
    'main{
      val evaled = exec('basic/"Main.sc")
      val out = evaled.out.string
      assert(out.contains("Hello! 1"))
    }
    'args{
      'full{
        val evaled = exec('basic/"Args.sc", "3", "Moo", (cwd/'omg/'moo).toString)
        assert(evaled.out.string.contains("Hello! MooMooMoo omg/moo."))
      }
      'default{
        val evaled = exec('basic/"Args.sc", "3", "Moo")
        assert(evaled.out.string.contains("Hello! MooMooMoo ."))
      }
      // Need a way for `%%` to capture stderr before we can specify these
      // tests a bit more tightly; currently the error just goes to stdout
      // and there's no way to inspect/validate it =/
      'tooFew{
        val errorMsg = intercept[ShelloutException]{
          exec('basic/"Args.sc", "3")
        }.result.err.string
        assert(errorMsg.contains(
          """The following arguments failed to be parsed:
            |(s: String) was missing
            |expected arguments: (i: Int, s: String, path: ammonite.ops.Path)"""
            .stripMargin
            .replace("\n", Util.newLine)
        ))
      }
      'cantParse{
        val errorMsg = intercept[ShelloutException]{
          exec('basic/"Args.sc", "foo", "moo")
        }.result.err.string
        val exMsg = """java.lang.NumberFormatException: For input string: "foo""""
        assert(errorMsg.contains(
          s"""The following arguments failed to be parsed:
             |(i: Int) failed to parse input "foo" with $exMsg
             |expected arguments: (i: Int, s: String, path: ammonite.ops.Path)"""
            .stripMargin
            .replace("\n", Util.newLine)
        ))
        // Ensure we're properly truncating the random stuff we don't care about
        // which means that the error stack that gets printed is short-ish
        assert(errorMsg.lines.length < 12)

      }
    }

    'load_script{
      val name = 'basic/"QuickSort.sc"
      val res = %%bash(
        executable,
        "--predef-file",
        emptyPrefdef,
        replStandaloneResources/name,
        "-t"
        )
      println("Time analysis of loading qs.sc(test script)\n\n")
      res.out.lines.foreach { println }
      println("\n-------------------------")
    }
  }
}
