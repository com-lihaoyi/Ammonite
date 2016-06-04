package ammonite.integration
import ammonite.ops._
import utest._
import TestUtils._
import ammonite.ops.ImplicitWd._
object LineNumberTests extends TestSuite{
  val tests = this{

    def interceptException(name: RelPath) = {
      intercept[ShelloutException]{
        %%bash(
          executable,
          "--predef-file",
          emptyPrefdef,
          replStandaloneResources / name
          )
      }
    }



    'errorTest{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val name = 'lineNumbers/"ErrorLineNumberTest.scala"
      val e = intercept[ShelloutException]{
        %%bash(
          executable,
          "--predef-file",
          emptyPrefdef,
          replStandaloneResources / name
          )
      }
      val expectedErrorMsg =
        """Syntax Error: ("}" | `case`):5:24 ...")\n  }\n\n  d"
          |    printlnqs(unsorted))
          |                       ^""".stripMargin

      assert(e.toString.contains(expectedErrorMsg))

    }

    'multipleCompilationUnitErrorTest1{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val file = 'lineNumbers/"MultipleCompilationUnitErrorMsgTest1.scala"

      val e = intercept[ShelloutException]{
        %%bash(
          executable,
          "--predef-file",
          emptyPrefdef,
          replStandaloneResources / file
          )
      }
      val expectedErrorMsg =
        """Syntax Error: End:5:1 ..."}"
          |}
          |^""".stripMargin

      assert(e.toString.contains(expectedErrorMsg))
    }

    'multipleCompilationUnitErrorTest2{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val file = 'lineNumbers/"MultipleCompilationUnitErrorMsgTest2.scala"

      val e = intercept[ShelloutException]{
        %%bash(
          executable,
          "--predef-file",
          emptyPrefdef,
          replStandaloneResources / file
          )
      }
      val expectedErrorMsg =
        """Syntax Error: End:3:1 ..."}\n@\n1 + 1"
          |}
          |^""".stripMargin

      assert(e.toString.contains(expectedErrorMsg))
    }

    'compilationErrorWithCommentsAtTop{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val name = 'lineNumbers/"compilationErrorWithCommentsAtTop.scala"

      val e = interceptException(name)
      val expectedErrorMsg =
        """compilationErrorWithCommentsAtTop.scala:11: not found: value quicort
          |    quicort(unsorted.filter(_ < pivot)):::List(pivot):::""".stripMargin +
          """quicksort(unsorted.filter(_ > pivot))"""

      assert(e.toString.contains(expectedErrorMsg))
    }

    'compilationErrorInSecondBlock{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val name = 'lineNumbers/"compilationErrorInSecondBlock.scala"
      val e = interceptException(name)
      val expectedErrorMsg =
        """compilationErrorInSecondBlock.scala:14: not found: value printnl
          |val res_0 = printnl("OK")
          |            ^""".stripMargin

      assert(e.toString.contains(expectedErrorMsg))
    }

    'compilationErrorInFourthBlock{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val name = 'lineNumbers/"compilationErrorInFourthBlock.scala"
      val e = interceptException(name)
      val expectedErrorMsg =
        """compilationErrorInFourthBlock.scala:30: not found: value prinntl
          |val res = prinntl("Ammonite")
          |          ^""".stripMargin

      assert(e.toString.contains(expectedErrorMsg))
    }

    'compilationErrorInClass{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val name = 'lineNumbers/"compilationErrorInClass.scala"
      val e = interceptException(name)
      val expectedErrorMsg = "compilationErrorInClass.scala:17: value a is not a member of"

      assert(e.toString.contains(expectedErrorMsg))
    }

    'CompilationErrorLineNumberTest{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val name = 'lineNumbers/"CompilationErrorLineNumberTest.scala"
      val e = interceptException(name)
      val expectedErrMsg = """CompilationErrorLineNumberTest.scala:7: not found: value noSuchObject
                             |  val x = noSuchObject.badFunction
                             |          ^""".stripMargin

      assert(e.toString.contains(expectedErrMsg))
    }

    'RuntimeCompilationErrorLineNumberTest{
      //Make sure correct line numbers are printed when an erroneous script is executed
      val name = 'lineNumbers/"RuntimeCompilationErrorLineNumberTest.scala"
      val e = interceptException(name)
      val expectedErrorMsg = "(RuntimeCompilationErrorLineNumberTest.scala:6)"

      assert(e.toString.contains(expectedErrorMsg))
    }
  }
}
