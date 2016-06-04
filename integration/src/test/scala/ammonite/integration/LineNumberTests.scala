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
      'lineNumbers/"ErrorLineNumberTest.scala",
      """Syntax Error: ("}" | `case`):5:24 ...")\n  }\n\n  d"
        |    printlnqs(unsorted))
        |                       ^""".stripMargin
    )

    'multipleCompilationUnitErrorTest1 - checkErrorMessage(
      'lineNumbers/"MultipleCompilationUnitErrorMsgTest1.scala",
      """Syntax Error: End:5:1 ..."}"
        |}
        |^""".stripMargin
    )


    'multipleCompilationUnitErrorTest2 - checkErrorMessage(
      'lineNumbers/"MultipleCompilationUnitErrorMsgTest2.scala",
      """Syntax Error: End:3:1 ..."}\n@\n1 + 1"
        |}
        |^""".stripMargin
    )

    'compilationErrorWithCommentsAtTop - checkErrorMessage(
      'lineNumbers/"compilationErrorWithCommentsAtTop.scala",
      """compilationErrorWithCommentsAtTop.scala:11: not found: value quicort
        |    quicort(unsorted.filter(_ < pivot)):::List(pivot):::""".stripMargin +
        """quicksort(unsorted.filter(_ > pivot))"""
    )

    'compilationErrorInSecondBlock - checkErrorMessage(
      'lineNumbers/"compilationErrorInSecondBlock.scala",
      """compilationErrorInSecondBlock.scala:14: not found: value printnl
        |val res_0 = printnl("OK")
        |            ^""".stripMargin
    )

    'compilationErrorInFourthBlock - checkErrorMessage(
      'lineNumbers/"compilationErrorInFourthBlock.scala",
      """compilationErrorInFourthBlock.scala:30: not found: value prinntl
        |val res = prinntl("Ammonite")
        |          ^""".stripMargin
    )

    'compilationErrorInClass - checkErrorMessage(
      'lineNumbers/"compilationErrorInClass.scala",
      "compilationErrorInClass.scala:17: value a is not a member of"
    )

    'CompilationErrorLineNumberTest - checkErrorMessage(
      'lineNumbers/"CompilationErrorLineNumberTest.scala",
      """CompilationErrorLineNumberTest.scala:7: not found: value noSuchObject
        |  val x = noSuchObject.badFunction
        |          ^""".stripMargin
    )

    'RuntimeCompilationErrorLineNumberTest - checkErrorMessage(
      'lineNumbers/"RuntimeCompilationErrorLineNumberTest.scala",
      "(RuntimeCompilationErrorLineNumberTest.scala:6)"
    )
  }
}
