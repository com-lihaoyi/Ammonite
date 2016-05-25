package ammonite.integration

import utest._
import ammonite.ops._
import ammonite.ops.ImplicitWd._

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
    def exec(name: String, args: String*) = {
      %%bash(
        executable,
        "--predef-file",
        emptyPrefdef,
        replStandaloneResources/name,
        args
      )
    }

    def interceptException(name: String) = {
      intercept[ShelloutException]{
        %%bash(
          executable,
          "--predef-file",
          emptyPrefdef,
          replStandaloneResources / name
          )
      }
    }

    'hello{
      val evaled = exec("Hello.scala")
      assert(evaled.out.trim == "Hello World")
    }

    'complex{
      val evaled = exec("Complex.scala")
      assert(evaled.out.trim.contains("Spire Interval [0, 10]"))
    }

    'load_script{
      val name = "QuickSort.scala"
      val res = %%bash(
        executable,
        "--predef-file",
        emptyPrefdef,
        replStandaloneResources/name,
        "-t"
        )
      println("Time analysis of loading qs.scala(test script)\n\n")
      res.out.lines.foreach { println }
      println("\n-------------------------")
    }

    'errorTest{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val name = "ErrorLineNumberTest.scala"
      val e = intercept[ShelloutException]{
        %%bash(
          executable,
          "--predef-file",
          emptyPrefdef,
          replStandaloneResources / name
          )
      }
      val expectedErrorMsg =
        """ammonite.repl.CompilationError: Syntax Error: ("}" | `case`):5:24 ...")\n  }\n\n  d"
          |    printlnqs(unsorted))
          |                       ^""".stripMargin

      assert(e.toString.contains(expectedErrorMsg))

    }

    'multipleCompilationUnitErrorTest1{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val file = "MultipleCompilationUnitErrorMsgTest1.scala"

      val e = intercept[ShelloutException]{
        %%bash(
          executable,
          "--predef-file",
          emptyPrefdef,
          replStandaloneResources / file
          )
      }
      val expectedErrorMsg =
        """ammonite.repl.CompilationError: Syntax Error: End:5:1 ..."}"
          |}
          |^""".stripMargin

      assert(e.toString.contains(expectedErrorMsg))
    }

    'multipleCompilationUnitErrorTest2{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val file = "MultipleCompilationUnitErrorMsgTest2.scala"

      val e = intercept[ShelloutException]{
        %%bash(
          executable,
          "--predef-file",
          emptyPrefdef,
          replStandaloneResources / file
          )
      }
      val expectedErrorMsg =
        """ammonite.repl.CompilationError: Syntax Error: End:3:1 ..."}\n@\n1 + 1"
          |}
          |^""".stripMargin

      assert(e.toString.contains(expectedErrorMsg))
    }

    'compilationErrorWithCommentsAtTop{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val name = "compilationErrorWithCommentsAtTop.scala"
      val e = interceptException(name)
      val expectedErrorMsg =
        """Main.scala:11: not found: value quicort
          |    quicort(unsorted.filter(_ < pivot)):::List(pivot):::""".stripMargin +
          """quicksort(unsorted.filter(_ > pivot))"""

      assert(e.toString.contains(expectedErrorMsg))
    }

    'compilationErrorInSecondBlock{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val name = "compilationErrorInSecondBlock.scala"
      val e = interceptException(name)
      val expectedErrorMsg =
        """Main.scala:14: not found: value printnl
          |val res_0 = printnl("OK")
          |            ^""".stripMargin

      assert(e.toString.contains(expectedErrorMsg))
    }

    'compilationErrorInFourthBlock{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val name = "compilationErrorInFourthBlock.scala"
      val e = interceptException(name)
      val expectedErrorMsg =
        """Main.scala:30: not found: value prinntl
          |val res = prinntl("Ammonite")
          |          ^""".stripMargin

      assert(e.toString.contains(expectedErrorMsg))
    }

    'compilationErrorInClass{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val name = "compilationErrorInClass.scala"
      val e = interceptException(name)
      val expectedErrorMsg = "Main.scala:17: value a is not a member of"

      assert(e.toString.contains(expectedErrorMsg))
    }

    'CompilationErrorLineNumberTest{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val name = "CompilationErrorLineNumberTest.scala"
      val e = interceptException(name)
      val expectedErrorMsg = """Main.scala:7: not found: value noSuchObject
                               |  val x = noSuchObject.badFunction
                               |          ^""".stripMargin

      assert(e.toString.contains(expectedErrorMsg))
    }

    'RuntimeCompilationErrorLineNumberTest{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val name = "RuntimeCompilationErrorLineNumberTest.scala"
      val e = interceptException(name)
      val expectedErrorMsg = "(Main.scala:6)"

      assert(e.toString.contains(expectedErrorMsg))
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


      val output = res.out.trim
      assert(output == "repl/src")
    }
    'main{
      val evaled = exec("Main.scala")
      assert(evaled.out.string.contains("Hello! 1"))
    }
    'classloaders{
      val evaled = exec("Resources.scala")
      assert(evaled.out.string.contains("1745"))
    }
//    'playframework{
//      val evaled = exec("PlayFramework.scala")
//      assert(evaled.out.string.contains("Hello bar"))
//    }
    'args{
      'full{
        val evaled = exec("Args.scala", "3", "Moo", (cwd/'omg/'moo).toString)
        assert(evaled.out.string.contains("Hello! MooMooMoo omg/moo."))
      }
      'default{
        val evaled = exec("Args.scala", "3", "Moo")
        assert(evaled.out.string.contains("Hello! MooMooMoo ."))
      }
      // Need a way for `%%` to capture stderr before we can specify these
      // tests a bit more tightly; currently the error just goes to stdout
      // and there's no way to inspect/validate it =/
      'tooFew{
        val errorMsg = intercept[ShelloutException]{
          exec("Args.scala", "3")
        }.result.err.string
        assert(errorMsg.contains("Unspecified value parameter s"))
      }
      'cantParse{
        val errorMsg = intercept[ShelloutException]{
          exec("Args.scala", "foo", "moo")
        }.result.err.string
        assert(errorMsg.contains("Cannot parse value \"foo\" into arg `i: Int`"))
        // Ensure we're properly truncating the random stuff we don't care about
        // which means that the error stack that gets printed is short-ish
        assert(errorMsg.lines.length < 12)

      }
    }
  }
}
