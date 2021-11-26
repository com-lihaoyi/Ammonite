package ammonite.integration

import ammonite.integration.TestUtils._
import ammonite.util.Util
import utest._

/**
  * Make sure that if we run Scala scripts using the Ammonite executable, and
  * they, fail with "expected" failure modes, don't show useless stack traces
  * and just show what the user did wrong
 */
object ErrorTruncationTests extends TestSuite{

  def checkErrorMessage(file: os.RelPath, expected: String): Unit = {
    val e = fansi.Str(
      Util.normalizeNewlines(
        intercept[os.SubprocessException]{ exec(file) }
          .result
          .err
          .string
      )
    ).plainText
    //This string gets included on windows due to environment variable set additionally

    assert(fansi.Str(e).plainText.contains(expected))
  }
  val tests = Tests {
    println("ErrorTruncationTests")
    test("compileError") - checkErrorMessage(
      file = os.rel/'errorTruncation/"compileError.sc",
      expected = Util.normalizeNewlines(
        if (isScala2)
          s"""compileError.sc:1: not found: value doesntexist
            |val res = doesntexist
            |          ^
            |Compilation Failed
            |""".stripMargin
        else
          s"""|   |val res = doesntexist
              |   |          ^^^^^^^^^^^
              |   |          Not found: doesntexist
              |Compilation Failed""".stripMargin
      )
    )
    test("multiExpressionError") - checkErrorMessage(
      file = os.rel / 'errorTruncation/"compileErrorMultiExpr.sc",
      expected = Util.normalizeNewlines(
        if (isScala2)
          s"""compileErrorMultiExpr.sc:11: not found: value doesntexist
            |val res_4 = doesntexist
            |            ^
            |Compilation Failed
            |""".stripMargin
        else
          s"""|   |val res_4 = doesntexist
              |   |            ^^^^^^^^^^^
              |   |            Not found: doesntexist
              |Compilation Failed""".stripMargin
      )
    )

    test("parseError"){
      if(!Util.windowsPlatform){
        checkErrorMessage(
          file = os.rel/'errorTruncation/"parseError.sc",
          expected = Util.normalizeNewlines(
            if (isScala2)
              """parseError.sc:1:1 expected end-of-input
                |}
                |^
                |""".stripMargin
            else
              """|  |}
                 |  |^
                 |  |eof expected, but '}' found""".stripMargin
          )
        )
      }
    }
    val tab = '\t'
    val runtimeErrorResourcePackage =
      "ammonite.$file.integration.src.test.resources.ammonite.integration.errorTruncation"

    test("runtimeError") - checkErrorMessage(
      file = os.rel/'errorTruncation/"runtimeError.sc",
      expected = Util.normalizeNewlines(
        if (scalaVersion.startsWith("2.12"))
          s"""java.lang.ArithmeticException: / by zero
             |  $runtimeErrorResourcePackage.runtimeError$$.<init>(runtimeError.sc:1)
             |  $runtimeErrorResourcePackage.runtimeError$$.<clinit>(runtimeError.sc)
             |""".stripMargin
        else if (isScala2)
          s"""java.lang.ArithmeticException: / by zero
             |  $runtimeErrorResourcePackage.runtimeError$$.<clinit>(runtimeError.sc:1)
             |""".stripMargin
        else
          // FIXME Line number is wrong
          s"""java.lang.ArithmeticException: / by zero
             |  $runtimeErrorResourcePackage.runtimeError$$.<clinit>(runtimeError.sc:42)
             |""".stripMargin
      )
    )
  }
}
