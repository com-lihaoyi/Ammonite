package ammonite.integration

import ammonite.integration.TestUtils._
import ammonite.ops.ImplicitWd._
import ammonite.ops._
import ammonite.util.Util
import utest._

/**
  * Tests around Ammonite's CLI handling of main methods, argument parsing,
  * and the associated error behavior if the caller messes up.
 */
object MainTests extends TestSuite{
  override def utestTruncateLength = 60000

  val tests = TestSuite {
    println("Running MainTests")

    'hello{
      val evaled = exec('basic/"Hello.sc")
      assert(evaled.out.trim == "Hello World")
    }

    // Not really related to main methods, but related since most of the main
    // logic revolves around handling arguments. Make sure this fails properly
    'badAmmoniteFlag{
      val evaled = intercept[ShelloutException]{
        execBase('basic/"Hello.sc", Seq("--doesnt-exist"), Nil)
      }.result
      val expected = "Unknown Ammonite option: --doesnt-exist"
      assert(evaled.err.toString.contains(expected))
    }
    //make sure scripts with symbols in path names work fine

    'main{
      'single{
        val evaled = exec('basic/"Main.sc")
        val out = evaled.out.string
        assert(out.contains("Hello! 1"))
      }
      'multiple{
        'positiveNoArgs{
          val evaled = exec('basic/"MultiMain.sc", "mainA")
          val out = evaled.out.string
          assert(out == "Hello! 1" + Util.newLine)
        }
        'positiveArgs{
          val evaled = exec('basic/"MultiMain.sc", "functionB", "2", "foo")
          val out = evaled.out.string
          assert(out == "Hello! foofoo ." + Util.newLine)
        }
        'specifyMain{
          val evaled = intercept[ShelloutException]{
            exec('basic/"MultiMain.sc")
          }.result
          val out = evaled.err.string
          val expected = Util.normalizeNewlines(
            s"""Need to specify a subcommand to call when running MultiMain.sc
                |
                |Available subcommands:
                |
                |def mainA()
                |def functionB(i: Int, s: String, path: ammonite.ops.Path = $pwd)
                |""".stripMargin
          )
          assert(out.contains(expected))
        }
        'cantFindMain{
          val evaled = intercept[ShelloutException]{
            exec('basic/"MultiMain.sc", "doesntExist")
          }.result
          val out = evaled.err.string
          val expected = Util.normalizeNewlines(
            s"""Unable to find subcommand: doesntExist
                |
                |Available subcommands:
                |
                |def mainA()
                |def functionB(i: Int, s: String, path: ammonite.ops.Path = $pwd)
                |""".stripMargin
          )
          assert(out.contains(expected))
        }
      }
    }

    'args{
      'full{
        val evaled = exec('basic/"Args.sc", "-i", "3", "--s", "Moo", (pwd/'omg/'moo).toString)
        assert(evaled.out.string == Util.normalizeNewlines("\"Hello! MooMooMoo moo.\"\n"))
      }

      'default{
        val evaled = exec('basic/"Args.sc", "3", "Moo")
        assert(
          evaled.out.string == Util.normalizeNewlines("\"Hello! MooMooMoo Ammonite.\"\n") ||
          // For some reason, on windows CI machines the repo gets clone as lowercase (???)
          evaled.out.string == Util.normalizeNewlines("\"Hello! MooMooMoo ammonite.\"\n")
        )
      }
      'manualPrintln{
        val evaled = exec('basic/"Args2.sc", "3", "Moo")
        assert(
          evaled.out.string == Util.normalizeNewlines("Hello! MooMooMoo Ammonite.\n") ||
          // For some reason, on windows CI machines the repo gets clone as lowercase (???)
          evaled.out.string == Util.normalizeNewlines("Hello! MooMooMoo ammonite.\n")
        )
      }
      'tooFew{
        val errorMsg = intercept[ShelloutException]{
          exec('basic/"Args.sc", "3")
        }.result.err.string

        assert(errorMsg.contains(
          Util.normalizeNewlines(
            s"""Arguments provided did not match expected signature:
               |def main(i: Int, s: String, path: ammonite.ops.Path = $pwd)
               |
               |Missing arguments: (s: String)""".stripMargin
          )
        ))
      }
      'varargs{
        // Make sure varargs are able to slurp up everything, including args
        // which start with `--`. This allows a user to define a main method
        // taking `String*`, slurping up all args un-changed, and then passing
        // them on to their own custom argument parsing code (e.g. scopt)
        val out = exec('basic/"Varargs.sc", "--i", "31337", "zomg", "--cow", "-omg", "bbq")
              .out.string

        assert(
          out.contains("31337"),
          out.contains("zomg"),
          out.contains("ArrayBuffer(--cow, -omg, bbq)")
        )
      }
      'argsGivenButNoMain{
        val errorMsg = intercept[ShelloutException]{
          exec('basic/"Hello.sc", "a", "b", "\"")
        }.result.err.string

        assert(errorMsg.contains(
          """Script Hello.sc does not take arguments: "a" "b" "\"""""
        ))
      }
      'tooMany{
        val errorMsg = intercept[ShelloutException]{
          exec('basic/"Args.sc", "3", "4", "5", "6")
        }.result.err.string

        assert(errorMsg.contains(
          Util.normalizeNewlines(
            s"""Arguments provided did not match expected signature:
                |def main(i: Int, s: String, path: ammonite.ops.Path = $pwd)
                |
                |Unknown arguments: "6"""".stripMargin
          )
        ))
      }
      'cantParse{
        val errorMsg = intercept[ShelloutException]{
          exec('basic/"Args.sc", "foo", "moo")
        }.result.err.string

        val exMsg = """java.lang.NumberFormatException: For input string: "foo""""
        assert(errorMsg.contains(
          Util.normalizeNewlines(
            s"""The following arguments failed to be parsed:
               |(i: Int) failed to parse input "foo" with $exMsg
               |expected signature: def main(i: Int, s: String, path: ammonite.ops.Path = $pwd)"""
              .stripMargin
          )
        ))
        // Ensure we're properly truncating the random stuff we don't care about
        // which means that the error stack that gets printed is short-ish
        assert(errorMsg.lines.length < 12)

      }
    }
  }
}
