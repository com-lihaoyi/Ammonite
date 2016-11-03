package ammonite.integration

import ammonite.integration.TestUtils._
import ammonite.ops.ImplicitWd._
import ammonite.ops._
import ammonite.util.Util
import utest._

/**
  * Make sure that if we run Scala scripts using the Ammonite executable, and
  * they, fail with "expected" failure modes, don't show useless stack traces
  * and just show what the user did wrong
 */
object ErrorTruncationTests extends TestSuite{
  override def utestTruncateLength = 60000

  def checkErrorMessage(file: RelPath, expected: String) = {
    val e = fansi.Str(
      Util.normalizeNewlines(
        intercept[ShelloutException]{ exec(file) }
          .result
          .err
          .string
      )
    ).plainText
    //This string gets included on windows due to environment variable set additionally

    assert(e == expected)
  }
  def scriptFile(name: String): Path =
    replStandaloneResources/'errorTruncation/name

  val tests = TestSuite {
    println("ErrorTruncationTests")
    'compileError - checkErrorMessage(
      file = 'errorTruncation/"compileError.sc",
      expected = Util.normalizeNewlines(
        s"""${scriptFile("compileError.sc")}:1: not found: value doesntexist
          |val res = doesntexist
          |          ^
          |Compilation Failed
          |""".stripMargin
      )
    )
    'multiExpressionError - checkErrorMessage(
      file = 'errorTruncation/"compileErrorMultiExpr.sc",
      expected = Util.normalizeNewlines(
        s"""${scriptFile("compileErrorMultiExpr.sc")}:11: not found: value doesntexist
          |val res_4 = doesntexist
          |            ^
          |Compilation Failed
          |""".stripMargin
      )
    )

    'parseError - {
      if(!Util.windowsPlatform){
        checkErrorMessage(
          file = 'errorTruncation/"parseError.sc",
          expected = Util.normalizeNewlines(
            """Syntax Error: End:1:1 ..."}\n"
              |}
              |^
              |""".stripMargin
          )
        )
      }
    }
    val tab = '\t'
    val runtimeErrorResourcePackage =
      "$file.integration.src.test.resources.ammonite.integration.errorTruncation"
    val runtimeErrorSc = scriptFile("runtimeError.sc")
    'runtimeError - checkErrorMessage(
      file = 'errorTruncation/"runtimeError.sc",
      expected = Util.normalizeNewlines(
        s"""Exception in thread "main" java.lang.ArithmeticException: / by zero
          |${tab}at $runtimeErrorResourcePackage.runtimeError$$.<init>($runtimeErrorSc:1)
          |${tab}at $runtimeErrorResourcePackage.runtimeError$$.<clinit>($runtimeErrorSc)
          |${tab}at $runtimeErrorResourcePackage.runtimeError.$$main($runtimeErrorSc)
          |""".stripMargin
      )
    )
  }
}
