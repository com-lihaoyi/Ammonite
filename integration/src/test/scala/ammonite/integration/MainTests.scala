package ammonite.integration

import ammonite.integration.TestUtils._
import ammonite.ops.ImplicitWd._
import ammonite.ops._
import ammonite.util.Util
import utest._

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
object MainTests extends TestSuite{
  override def utestTruncateLength = 60000

  val tests = TestSuite {
    println("Running MainTests")

    'hello{
      val evaled = exec('basic/"Hello.sc")
      assert(evaled.out.trim == "Hello World")
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
        val evaled = exec('basic/"Args.sc", "3", "Moo", (pwd/'omg/'moo).toString)
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
               |(i: Int, s: String, path: ammonite.ops.Path = $pwd)
               |
               |Missing arguments: (s: String)""".stripMargin
          )
        ))
      }
      'tooMany{
        val errorMsg = intercept[ShelloutException]{
          exec('basic/"Args.sc", "3", "4", "5", "6")
        }.result.err.string

        assert(errorMsg.contains(
          Util.normalizeNewlines(
            s"""Arguments provided did not match expected signature:
                |(i: Int, s: String, path: ammonite.ops.Path = $pwd)
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
               |expected arguments: (i: Int, s: String, path: ammonite.ops.Path = $pwd)"""
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
