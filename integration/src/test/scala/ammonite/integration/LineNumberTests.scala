package ammonite.integration
import ammonite.ops._
import utest._
import TestUtils._

/**
  * Make sure that when a script is run with parse errors, compile errors
  * or runtime errors, the line numbers in the error message match up with
  * the correct line numbers in the original script and not the line numbers
  * from the mangled/preprocessed code
  */
object LineNumberTests extends TestSuite{
  val tests = this{

    def checkErrorMessage(file: RelPath, expected: String) = {
      val e = intercept[ShelloutException]{ exec(file) }.result.err.string
      assert(e.contains(expected))
    }

    'errorTest - checkErrorMessage(
      file = 'lineNumbers/"ErrorLineNumberTest.sc",
      expected =
        """Syntax Error: ("}" | `case`):5:24 ...")\n  }\n\n  d"
          |    printlnqs(unsorted))
          |                       ^""".stripMargin
    )

    'multipleCompilationUnitErrorTest1 - checkErrorMessage(
      file = 'lineNumbers/"MultipleCompilationUnitErrorMsgTest1.sc",
      expected =
        """Syntax Error: End:5:1 ..."}"
          |}
          |^""".stripMargin
    )


    'multipleCompilationUnitErrorTest2 - checkErrorMessage(
      file = 'lineNumbers/"MultipleCompilationUnitErrorMsgTest2.sc",
      expected =
        """Syntax Error: End:3:1 ..."}\n@\n1 + 1"
          |}
          |^""".stripMargin
    )

    'compilationErrorWithCommentsAtTop - checkErrorMessage(
      file = 'lineNumbers/"compilationErrorWithCommentsAtTop.sc",
      expected =
        """compilationErrorWithCommentsAtTop.sc:11: not found: value quicort
          |    quicort(unsorted.filter(_ < pivot)):::List(pivot):::""".stripMargin +
        """quicksort(unsorted.filter(_ > pivot))"""
    )

    'compilationErrorInSecondBlock - checkErrorMessage(
      file = 'lineNumbers/"compilationErrorInSecondBlock.sc",
      expected =
        """compilationErrorInSecondBlock.sc:14: not found: value printnl
          |val res_0 = printnl("OK")
          |            ^""".stripMargin
    )

    'compilationErrorInFourthBlock - checkErrorMessage(
      file = 'lineNumbers/"compilationErrorInFourthBlock.sc",
      expected =
        """compilationErrorInFourthBlock.sc:30: not found: value prinntl
          |val res = prinntl("Ammonite")
          |          ^""".stripMargin
    )

    'compilationErrorInClass - checkErrorMessage(
      file = 'lineNumbers/"compilationErrorInClass.sc",
      expected = "compilationErrorInClass.sc:17: value a is not a member of"
    )

    'CompilationErrorLineNumberTest - checkErrorMessage(
      file = 'lineNumbers/"CompilationErrorLineNumberTest.sc",
      expected =
        """CompilationErrorLineNumberTest.sc:7: not found: value noSuchObject
          |  val x = noSuchObject.badFunction
          |          ^""".stripMargin
    )

    'RuntimeCompilationErrorLineNumberTest - checkErrorMessage(
      file = 'lineNumbers/"RuntimeCompilationErrorLineNumberTest.sc",
      expected = "(RuntimeCompilationErrorLineNumberTest.sc:6)"
    )
  }
}
