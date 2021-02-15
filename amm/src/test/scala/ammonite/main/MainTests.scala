package ammonite.main

import ammonite.TestUtils

import ammonite.util.Util
import utest._

/**
  * Tests around Ammonite's CLI handling of main methods, argument parsing,
  * and the associated error behavior if the caller messes up.
 */
class MainTests extends TestSuite{
  def exec(p: String, args: String*) =
    new InProcessMainMethodRunner(os.rel / 'mains / p, Nil, args)

  def stripInvisibleMargin(s: String): String = {
    val lines = Predef.augmentString(s).lines.toArray
    val leftMargin = lines.filter(_.trim.nonEmpty).map(_.takeWhile(_ == ' ').length).min
    lines.map(_.drop(leftMargin)).mkString(Util.newLine)
  }

  def tests = Tests {
    println("Running MainTests")

    test("hello"){
      val evaled = exec("Hello.sc")
      assert(evaled.out.trim == "Hello World")
    }

    test("compilerCrash"){
      if(TestUtils.scala2_11){
        val evaled = exec("CompilerCrash.sc")
        // Make sure we do not accidentally lose the stack trace in the case
        // where the script fails during compilation before entering the evaluator
        assert(Predef.augmentString(evaled.err).lines.length > 50)
      }
    }

    // Not really related to main methods, but related since most of the main
    // logic revolves around handling arguments. Make sure this fails properly
    test("badAmmoniteFlag"){
      val evaled = new InProcessMainMethodRunner(
        os.rel / 'mains/"Hello.sc",
        List("--doesnt-exist"),
        Nil
      )
      assert(!evaled.success)
      val expected = "Unknown Ammonite option: --doesnt-exist"
      assert(evaled.err.toString.contains(expected))
    }
    //make sure scripts with symbols in path names work fine

    test("main"){
      test("single"){
        val evaled = exec("Main.sc")
        assert(evaled.success)
        val out = evaled.out
        assert(out.contains("Hello! 1"))
      }
      test("multiple"){
        test("positiveNoArgs"){
          val evaled = exec("MultiMain.sc", "mainA")
          assert(evaled.success)
          val out = evaled.out
          assert(out == "Hello! 1" + Util.newLine)
        }
        test("positiveArgs"){
          val evaled = exec("MultiMainDoc.sc", "functionB", "2", "foo")
          assert(evaled.success)
          val out = evaled.out
          assert(out == "Hello! foofoo ." + Util.newLine)
        }
        test("specifyMain"){
          val evaled = exec("MultiMain.sc")
          assert(!evaled.success)
          val out = evaled.err
          val expected = stripInvisibleMargin(
            "Need to specify a sub command: mainA, functionB"
          )
          assert(out.contains(expected.trim))
        }
        test("specifyMainDoc"){
          val evaled = exec("MultiMainDoc.sc")
          assert(!evaled.success)
          val out = evaled.err
          val expected = stripInvisibleMargin(
            "Need to specify a sub command: mainA, functionB"
          )
          assert(out.contains(expected.trim))
        }
        test("cantFindMain"){
          val evaled = exec("MultiMainDoc.sc", "doesntExist")
          assert(!evaled.success)
          val out = evaled.err
          val expected = stripInvisibleMargin(
            "Unable to find subcommand: doesntExist, available subcommands: mainA, functionB"
          )
          assert(out.contains(expected.trim))
        }
        test("emptyArg"){
          val evaled = exec("ArgList.sc", "")
          assert(evaled.success)
        }
      }
    }

    test("args"){
      test("full"){
        val evaled = exec("Args.sc", "-i", "3", "-s", "Moo", (os.pwd/'omg/'moo).toString)
        assert(evaled.success)
        assert(evaled.out == ("\"Hello! MooMooMoo moo.\"" + Util.newLine))
      }

      test("default"){
        val evaled = exec("Args.sc", "3", "Moo")
        assert(evaled.success)
        assert(
          evaled.out == ("\"Hello! MooMooMoo Ammonite.\"" + Util.newLine) ||
          // For some reason, on windows CI machines the repo gets clone as lowercase (???)
          evaled.out == ("\"Hello! MooMooMoo ammonite.\"" + Util.newLine)
        )
      }
      test("manualPrintln"){
        val evaled = exec("Args2.sc", "3", "Moo")
        assert(evaled.success)
        assert(
          evaled.out == ("Hello! MooMooMoo Ammonite." + Util.newLine) ||
          // For some reason, on windows CI machines the repo gets clone as lowercase (???)
          evaled.out == ("Hello! MooMooMoo ammonite." + Util.newLine)
        )
      }
      val argsUsageMsg =
        s"""Expected Signature: main
           |  -i <int>
           |  -s <str>
           |  --path <path>""".stripMargin
      test("tooFew"){
        val evaled = exec("Args.sc", "3")
        assert(!evaled.success)

        assert(evaled.err.contains(
          Util.normalizeNewlines(
            s"""Missing argument: -s <str>
               |$argsUsageMsg""".stripMargin
          )
        ))
      }
      test("badHalfFlag"){
        // Make sure if someone passes in a flag without a corresponding RHS
        // value, it gets treated as a keyword rather than a dumb parameter
        // and raises an error if it doesn't exist
        val evaled = exec("Args.sc", "3", "lol", "-doesntexist")
        assert(!evaled.success)

        assert(evaled.err.contains(
          Util.normalizeNewlines(
            s"""Unknown argument: "-doesntexist"
               |$argsUsageMsg""".stripMargin
          )
        ))
      }
      test("goodHalfFlag"){
        // Make sure if someone passes in a flag without a corresponding RHS
        // value, it gets treated as a keyword rather than a dumb parameter
        // and raises an error if it doesn't exist
        val evaled = exec("Args.sc", "3", "-s")
        assert(!evaled.success)

        assert(evaled.err.contains(
          Util.normalizeNewlines(
            s"""Incomplete argument -s <str> is missing a corresponding value
               |$argsUsageMsg""".stripMargin
          )
        ))
      }
      test("varargs"){
        // Make sure varargs are able to slurp up everything, including args
        // which start with `--`. This allows a user to define a main method
        // taking `String*`, slurping up all args un-changed, and then passing
        // them on to their own custom argument parsing code (e.g. scopt)
        val evaled = exec("Varargs.sc",
          // Normal args get fulfilled
          "-i", "31337", "zomg",
          // Make sure single-dash -cow has the single-dash preserved
          "-cow", "--omg",
          // Random non-keyword args get passed straight through
          "bbq",
          // Keyword args that match an earlier argument get passed through too
          "-i", "x",
          // And so do flags without a paired argument
          "--i"
        )

        assert(evaled.success)
        val out = evaled.out
        assert(
          out.contains("31337"),
          out.contains("zomg"),
          out.contains("List(-cow, --omg, bbq, -i, x, --i)")
        )
      }
      test("argsGivenButNoMain"){
        val evaled = exec("Hello.sc", "a", "b", "\"")
        assert(!evaled.success)

        assert(evaled.err.contains(
          """Script Hello.sc does not take arguments: "a" "b" "\"""""
        ))
      }
      test("tooMany"){
        val evaled = exec("Args.sc", "3", "4", "5", "6", "7")
        assert(!evaled.success)

        assert(evaled.err.contains(
          Util.normalizeNewlines(
            s"""Unknown arguments: "6" "7"
               |$argsUsageMsg""".stripMargin
          )
        ))
      }
      test("multipleErrors"){
        val evaled = exec("Args.sc", "3", "-i", "4", "--unknown", "6")
        assert(!evaled.success)

        assert(evaled.err.contains(
          Util.normalizeNewlines(
            s"""Missing argument: -s <str>
               |Unknown arguments: "--unknown" "6"
               |Duplicate arguments for -i <int>: "3" "4"
               |$argsUsageMsg""".stripMargin
          )
        ))
      }
      test("cantParse"){
        val evaled = exec("Args.sc", "foo", "moo")
        assert(!evaled.success)

        val exMsg = """java.lang.NumberFormatException: For input string: "foo""""

        assert(evaled.err.contains(
          Util.normalizeNewlines(
            s"""Invalid argument -i <int> failed to parse "foo" due to $exMsg
               |Expected Signature: main
               |  -i <int>
               |  -s <str>
               |  --path <path>
               |""".stripMargin
          )
        ))
        // Ensure we're properly truncating the random stuff we don't care about
        // which means that the error stack that gets printed is short-ish
        assert(Predef.augmentString(evaled.err).lines.length < 20)

      }
    }
  }
}

object MainTests extends MainTests {
  val isScala2 = ammonite.compiler.CompilerBuilder.scalaVersion.startsWith("2.")
  override def tests =
    if (isScala2) super.tests
    else
      Tests {
        test("Disabled in Scala 3") - "Disabled in Scala 3"
      }
}
