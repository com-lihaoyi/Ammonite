package ammonite.main

import ammonite.ops._
import ammonite.util.Util
import utest._

/**
  * Make sure that when a script is run with parse errors, compile errors
  * or runtime errors, the line numbers in the error message match up with
  * the correct line numbers in the original script and not the line numbers
  * from the mangled/preprocessed code
  */
object LineNumberTests extends TestSuite{
  val tests = this{

    def checkErrorMessage(file: RelPath, expected: String) = {
      val e = new InProcessMainMethodRunner(file, Nil, Nil)

      assert(e.err.contains(expected))
    }

    //All Syntax Error tests currently don't pass on windows as fastparse gives out some 10
    //surrounding chars which are different on windows and linux due to `\n` and `\r\n`
    //as `\r\n` counts as 2 so less number of surrounding chars are shown on windows
    'errorTest - {
      if(!Util.windowsPlatform) {
        checkErrorMessage(
          file = 'lineNumbers / "ErrorLineNumberTest.sc",
          expected = Util.normalizeNewlines(
            """Syntax Error: "}":5:24 ...")\n  }\n\n  d"
              |    printlnqs(unsorted))
              |                       ^""".stripMargin
          )
        )
      }
    }

    'multipleCompilationUnitErrorTest1 - {
      if(!Util.windowsPlatform) {
        checkErrorMessage(
          file = 'lineNumbers/"MultipleCompilationUnitErrorMsgTest1.sc",
          expected = Util.normalizeNewlines(
            """Syntax Error: End:5:1 ..."}"
              |}
              |^""".stripMargin
          )
        )
      }
    }


    'multipleCompilationUnitErrorTest2 - {
      if(!Util.windowsPlatform) {
        checkErrorMessage(
          file = 'lineNumbers/"MultipleCompilationUnitErrorMsgTest2.sc",
          expected = Util.normalizeNewlines(
            """Syntax Error: End:3:1 ..."}\n@\n1 + 1"
              |}
              |^""".stripMargin
          )
        )
      }
    }

    'compilationErrorWithCommentsAtTop - checkErrorMessage(
      file = 'lineNumbers/"compilationErrorWithCommentsAtTop.sc",
      expected = Util.normalizeNewlines(
        """compilationErrorWithCommentsAtTop.sc:11: not found: value quicort
          |    quicort(unsorted.filter(_ < pivot)):::List(pivot):::""".stripMargin +
        """quicksort(unsorted.filter(_ > pivot))"""
      )
    )

    'compilationErrorInSecondBlock - checkErrorMessage(
      file = 'lineNumbers/"compilationErrorInSecondBlock.sc",
      expected = Util.normalizeNewlines(
        """compilationErrorInSecondBlock.sc:14: not found: value printnl
          |val res_0 = printnl("OK")
          |            ^""".stripMargin
      )
    )

    'compilationErrorInFourthBlock - checkErrorMessage(
      file = 'lineNumbers/"compilationErrorInFourthBlock.sc",
      expected = Util.normalizeNewlines(
        """compilationErrorInFourthBlock.sc:30: not found: value prinntl
          |val res = prinntl("Ammonite")
          |          ^""".stripMargin
      )
    )

    'compilationErrorInClass - checkErrorMessage(
      file = 'lineNumbers/"compilationErrorInClass.sc",
      expected = "compilationErrorInClass.sc:17: value a is not a member of"
    )

    'CompilationErrorLineNumberTest - checkErrorMessage(
      file = 'lineNumbers/"CompilationErrorLineNumberTest.sc",
      expected = Util.normalizeNewlines(
        """CompilationErrorLineNumberTest.sc:7: not found: value noSuchObject
          |  val x = noSuchObject.badFunction
          |          ^""".stripMargin
      )
    )

    'RuntimeCompilationErrorLineNumberTest - checkErrorMessage(
      file = 'lineNumbers/"RuntimeCompilationErrorLineNumberTest.sc",
      expected = {
        val p = InProcessMainMethodRunner.base/'lineNumbers
        s"${p/"RuntimeCompilationErrorLineNumberTest.sc"}:6)"
      }
    )
  }
}
