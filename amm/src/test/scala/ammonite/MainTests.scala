package ammonite

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import ammonite.ops._
import ammonite.util.Util
import utest._

/**
  * Tests around Ammonite's CLI handling of main methods, argument parsing,
  * and the associated error behavior if the caller messes up.
 */
object MainTests extends TestSuite{
  def exec(p: RelPath, args: String*) = new Executor(p, Nil, args)
  class Executor(p: RelPath, preArgs: List[String], args: Seq[String]){
    val in = new ByteArrayInputStream(Array.empty[Byte])
    val err0 = new ByteArrayOutputStream()
    val out0 = new ByteArrayOutputStream()
    val path = pwd/'amm/'src/'test/'resources/'mains/p
    val result = Console.withIn(in){
      Console.withErr(err0){
        Console.withOut(out0){
          Main.main0(preArgs ++ Seq(path.toString) ++ args.toList, in, out0, err0)
        }
      }
    }
    val success = result match{
      case Right(success) => success
      case Left((success, msg)) =>
        if (success) out0.write(msg.getBytes)
        else err0.write(msg.getBytes)
        success
    }

    val out = new String(out0.toByteArray)
    val err = new String(err0.toByteArray)
    override def toString = {
      s"Executor($out, $err)"
    }
  }
  override def utestTruncateLength = 60000

  val tests = TestSuite {
    println("Running MainTests")

    'hello{
      val evaled = exec("Hello.sc")
      assert(evaled.out.trim == "Hello World")
    }

    // Not really related to main methods, but related since most of the main
    // logic revolves around handling arguments. Make sure this fails properly
    'badAmmoniteFlag{
      val evaled = new Executor("Hello.sc", List("--doesnt-exist"), Nil)
      assert(!evaled.success)
      val expected = "Unknown Ammonite option: --doesnt-exist"
      assert(evaled.err.toString.contains(expected))
    }
    //make sure scripts with symbols in path names work fine

    'main{
      'single{
        val evaled = exec("Main.sc")
        assert(evaled.success)
        val out = evaled.out
        assert(out.contains("Hello! 1"))
      }
      'multiple{
        'positiveNoArgs{
          val evaled = exec("MultiMain.sc", "mainA")
          assert(evaled.success)
          val out = evaled.out
          assert(out == "Hello! 1" + Util.newLine)
        }
        'positiveArgs{
          val evaled = exec("MultiMain.sc", "functionB", "2", "foo")
          assert(evaled.success)
          val out = evaled.out
          assert(out == "Hello! foofoo ." + Util.newLine)
        }
        'specifyMain{
          val evaled = exec("MultiMain.sc")
          assert(!evaled.success)
          val out = evaled.err
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
          val evaled = exec("MultiMain.sc", "doesntExist")
          assert(!evaled.success)
          val out = evaled.err
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
        val evaled = exec("Args.sc", "-i", "3", "--s", "Moo", (pwd/'omg/'moo).toString)
        assert(evaled.success)
        assert(evaled.out == Util.normalizeNewlines("\"Hello! MooMooMoo moo.\"\n"))
      }

      'default{
        val evaled = exec("Args.sc", "3", "Moo")
        assert(evaled.success)
        assert(
          evaled.out == Util.normalizeNewlines("\"Hello! MooMooMoo Ammonite.\"\n") ||
          // For some reason, on windows CI machines the repo gets clone as lowercase (???)
          evaled.out == Util.normalizeNewlines("\"Hello! MooMooMoo ammonite.\"\n")
        )
      }
      'manualPrintln{
        val evaled = exec("Args2.sc", "3", "Moo")
        assert(evaled.success)
        assert(
          evaled.out == Util.normalizeNewlines("Hello! MooMooMoo Ammonite.\n") ||
          // For some reason, on windows CI machines the repo gets clone as lowercase (???)
          evaled.out == Util.normalizeNewlines("Hello! MooMooMoo ammonite.\n")
        )
      }
      'tooFew{
        val evaled = exec("Args.sc", "3")
        assert(!evaled.success)

        assert(evaled.err.contains(
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
        val evaled = exec("Varargs.sc", "--i", "31337", "zomg", "--cow", "-omg", "bbq")

        assert(evaled.success)
        val out = evaled.out
        assert(
          out.contains("31337"),
          out.contains("zomg"),
          out.contains("ArrayBuffer(--cow, -omg, bbq)")
        )
      }
      'argsGivenButNoMain{
        val evaled = exec("Hello.sc", "a", "b", "\"")
        assert(!evaled.success)

        assert(evaled.err.contains(
          """Script Hello.sc does not take arguments: "a" "b" "\"""""
        ))
      }
      'tooMany{
        val evaled = exec("Args.sc", "3", "4", "5", "6")
        assert(!evaled.success)

        assert(evaled.err.contains(
          Util.normalizeNewlines(
            s"""Arguments provided did not match expected signature:
                |def main(i: Int, s: String, path: ammonite.ops.Path = $pwd)
                |
                |Unknown arguments: "6"""".stripMargin
          )
        ))
      }
      'cantParse{
        val evaled = exec("Args.sc", "foo", "moo")
        assert(!evaled.success)

        val exMsg = """java.lang.NumberFormatException: For input string: "foo""""
        assert(evaled.err.contains(
          Util.normalizeNewlines(
            s"""The following arguments failed to be parsed:
               |(i: Int) failed to parse input "foo" with $exMsg
               |expected signature: def main(i: Int, s: String, path: ammonite.ops.Path = $pwd)"""
              .stripMargin
          )
        ))
        // Ensure we're properly truncating the random stuff we don't care about
        // which means that the error stack that gets printed is short-ish
        assert(evaled.err.lines.length < 12)

      }
    }
  }
}
