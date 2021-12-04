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

    def checkErrorMessage(file: os.Path, expected: String): Unit = {
      val e = new InProcessMainMethodRunner(file, Nil, Nil)

      assert(e.err.contains(expected))
    }

    val sv = ammonite.compiler.CompilerBuilder.scalaVersion
    val isScala2 = sv.startsWith("2.")

    test("sourcecode"){
      val path = InProcessMainMethodRunner.base / 'lineNumbers / "sourceCodeMetadata.sc"
      checkErrorMessage(
        file = path,
        s"""sourceCodeMetadata.sc
           |/Users/lihaoyi/Github/Ammonite/amm/src/test/resources/lineNumbers/sourceCodeMetadata.sc
           |3
           |""".stripMargin
      )
    }
    //All Syntax Error tests currently don't pass on windows as fastparse gives out some 10
    //surrounding chars which are different on windows and linux due to `\n` and `\r\n`
    //as `\r\n` counts as 2 so less number of surrounding chars are shown on windows
    test("errorTest"){
      if(!Util.windowsPlatform) {
        val path = InProcessMainMethodRunner.base / 'lineNumbers / "ErrorLineNumberTest.sc"
        checkErrorMessage(
          file = path,
          expected = Util.normalizeNewlines(
            if (isScala2)
              s"""$path:5:24 expected "}"
                |    printlnqs(unsorted))
                |                       ^""".stripMargin
            else
              s"""$path
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
        val path = InProcessMainMethodRunner.base / 'lineNumbers/"MultipleCompilationUnitErrorMsgTest1.sc"
          checkErrorMessage(
          file = path,
          expected = Util.normalizeNewlines(
            if (isScala2)
              s"""$path:5:1 expected end-of-input
                |}
                |^""".stripMargin
            else
              s"""$path
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
        val path = InProcessMainMethodRunner.base / 'lineNumbers/"MultipleCompilationUnitErrorMsgTest2.sc"
        checkErrorMessage(
          file = path,
          expected = Util.normalizeNewlines(
            if (isScala2)
              s"""$path:3:1 expected end-of-input
                |}
                |^""".stripMargin
            else
              s"""$path
                |  |}
                |  |^
                |  |eof expected, but '}' found
                |""".stripMargin
          )
        )
      }
    }

    test("compilationErrorWithCommentsAtTop") {
      val path = InProcessMainMethodRunner.base / 'lineNumbers/"compilationErrorWithCommentsAtTop.sc"
      checkErrorMessage(
        file = path,
        expected = Util.normalizeNewlines(
          if (isScala2)
            s"""$path:11: not found: value quicort
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
    }

    test("compilationErrorInSecondBlock") {
      val path = InProcessMainMethodRunner.base / 'lineNumbers/"compilationErrorInSecondBlock.sc"
      checkErrorMessage(
        file = path,
        expected = Util.normalizeNewlines(
          if (isScala2)
            s"""$path:14: not found: value printnl
              |val res_0 = printnl("OK")
              |            ^""".stripMargin
          else
            """   |val res_0 = printnl("OK")
              |   |            ^^^^^^^
              |   |            Not found: printnl""".stripMargin
        )
      )
    }

    test("compilationErrorInFourthBlock") {
      val path = InProcessMainMethodRunner.base / 'lineNumbers/"compilationErrorInFourthBlock.sc"
      checkErrorMessage(
        file = path,
        expected = Util.normalizeNewlines(
          if (isScala2)
            s"""$path:30: not found: value prinntl
              |val res = prinntl("Ammonite")
              |          ^""".stripMargin
          else
            """   |val res = prinntl("Ammonite")
              |   |          ^^^^^^^
              |   |          Not found: prinntl""".stripMargin
        )
      )
    }

    test("compilationErrorInClass") {
      val path = InProcessMainMethodRunner.base / 'lineNumbers/"compilationErrorInClass.sc"
      checkErrorMessage(
        file = path,
        expected =
          if (isScala2)
            s"$path:17: value a is not a member of"
          else
            "value a cannot be accessed as a member of"
      )
    }

    test("CompilationErrorLineNumberTest") {
      val path = InProcessMainMethodRunner.base / 'lineNumbers / "CompilationErrorLineNumberTest.sc"
      checkErrorMessage(
        file = path,
        expected = Util.normalizeNewlines(
          if (isScala2)
            s"""$path:7: not found: value noSuchObject
              |  val x = noSuchObject.badFunction
              |          ^""".stripMargin
          else
            """   |  val x = noSuchObject.badFunction
              |   |          ^^^^^^^^^^^^
              |   |          Not found: noSuchObject""".stripMargin
        )
      )
    }

    test("RuntimeCompilationErrorLineNumberTest") - {
      if (isScala2)
        checkErrorMessage(
          file = InProcessMainMethodRunner.base / 'lineNumbers/"RuntimeCompilationErrorLineNumberTest.sc",
          expected = s"(RuntimeCompilationErrorLineNumberTest.sc:6)"
        )
      else
        "Temporarily disabled in Scala 3"
    }
  }
}
