package ammonite.main


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

    def checkErrorMessage(file: os.RelPath, expected: String): Unit = {
      val e = new InProcessMainMethodRunner(file, Nil, Nil)

      assert(e.err.contains(expected))
    }

    val sv = ammonite.compiler.CompilerBuilder.scalaVersion
    val isScala2 = sv.startsWith("2.")

    //All Syntax Error tests currently don't pass on windows as fastparse gives out some 10
    //surrounding chars which are different on windows and linux due to `\n` and `\r\n`
    //as `\r\n` counts as 2 so less number of surrounding chars are shown on windows
    test("errorTest"){
      if(!Util.windowsPlatform) {
        checkErrorMessage(
          file = os.rel / 'lineNumbers / "ErrorLineNumberTest.sc",
          expected = Util.normalizeNewlines(
            if (isScala2)
              """ErrorLineNumberTest.sc:5:24 expected "}"
                |    printlnqs(unsorted))
                |                       ^""".stripMargin
            else
              """ErrorLineNumberTest.sc
                |  |    printlnqs(unsorted))
                |  |                       ^
                |  |                       '}' expected, but ')' found
                |""".stripMargin
          )
        )
      }
    }

    test("multipleCompilationUnitErrorTest1"){
      if(!Util.windowsPlatform) {
        checkErrorMessage(
          file = os.rel / 'lineNumbers/"MultipleCompilationUnitErrorMsgTest1.sc",
          expected = Util.normalizeNewlines(
            if (isScala2)
              """MultipleCompilationUnitErrorMsgTest1.sc:5:1 expected end-of-input
                |}
                |^""".stripMargin
            else
              """MultipleCompilationUnitErrorMsgTest1.sc
                |  |}
                |  |^
                |  |eof expected, but '}' found
                |""".stripMargin
          )
        )
      }
    }


    test("multipleCompilationUnitErrorTest2"){
      if(!Util.windowsPlatform) {
        checkErrorMessage(
          file = os.rel / 'lineNumbers/"MultipleCompilationUnitErrorMsgTest2.sc",
          expected = Util.normalizeNewlines(
            if (isScala2)
              """MultipleCompilationUnitErrorMsgTest2.sc:3:1 expected end-of-input
                |}
                |^""".stripMargin
            else
              """MultipleCompilationUnitErrorMsgTest2.sc
                |  |}
                |  |^
                |  |eof expected, but '}' found
                |""".stripMargin
          )
        )
      }
    }

    test("compilationErrorWithCommentsAtTop") - checkErrorMessage(
      file = os.rel / 'lineNumbers/"compilationErrorWithCommentsAtTop.sc",
      expected = Util.normalizeNewlines(
        if (isScala2)
          """compilationErrorWithCommentsAtTop.sc:11: not found: value quicort
            |    quicort(unsorted.filter(_ < pivot)):::List(pivot):::""".stripMargin +
          """quicksort(unsorted.filter(_ > pivot))"""
        else {
          val firstLine = "quicort(unsorted.filter(_ < pivot)):::List(pivot):::" +
            "quicksort(unsorted.filter(_ > pivot))"
          s"""|   |    $firstLine
              |   |    ^^^^^^^
              |   |    Not found: quicort""".stripMargin
        }
      )
    )

    test("compilationErrorInSecondBlock") - checkErrorMessage(
      file = os.rel / 'lineNumbers/"compilationErrorInSecondBlock.sc",
      expected = Util.normalizeNewlines(
        if (isScala2)
          """compilationErrorInSecondBlock.sc:14: not found: value printnl
            |val res_0 = printnl("OK")
            |            ^""".stripMargin
        else
          """   |val res_0 = printnl("OK")
            |   |            ^^^^^^^
            |   |            Not found: printnl""".stripMargin
      )
    )

    test("compilationErrorInFourthBlock") - checkErrorMessage(
      file = os.rel / 'lineNumbers/"compilationErrorInFourthBlock.sc",
      expected = Util.normalizeNewlines(
        if (isScala2)
          """compilationErrorInFourthBlock.sc:30: not found: value prinntl
            |val res = prinntl("Ammonite")
            |          ^""".stripMargin
        else
          """   |val res = prinntl("Ammonite")
            |   |          ^^^^^^^
            |   |          Not found: prinntl""".stripMargin
      )
    )

    test("compilationErrorInClass") - checkErrorMessage(
      file = os.rel / 'lineNumbers/"compilationErrorInClass.sc",
      expected =
        if (isScala2)
          "compilationErrorInClass.sc:17: value a is not a member of"
        else
          "value a cannot be accessed as a member of"
    )

    test("CompilationErrorLineNumberTest") - checkErrorMessage(
      file = os.rel / 'lineNumbers/"CompilationErrorLineNumberTest.sc",
      expected = Util.normalizeNewlines(
        if (isScala2)
          """CompilationErrorLineNumberTest.sc:7: not found: value noSuchObject
            |  val x = noSuchObject.badFunction
            |          ^""".stripMargin
        else
          """   |  val x = noSuchObject.badFunction
            |   |          ^^^^^^^^^^^^
            |   |          Not found: noSuchObject""".stripMargin
      )
    )

    test("RuntimeCompilationErrorLineNumberTest") - checkErrorMessage(
      file = os.rel / 'lineNumbers/"RuntimeCompilationErrorLineNumberTest.sc",
      expected = "(RuntimeCompilationErrorLineNumberTest.sc:6)"
    )
  }
}
