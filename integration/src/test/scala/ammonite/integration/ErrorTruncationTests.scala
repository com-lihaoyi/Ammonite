package ammonite.integration

import ammonite.integration.TestUtils._
import ammonite.ops.ImplicitWd._
import ammonite.ops._
import utest._

/**
  * Make sure that if we run Scala scripts using the Ammonite executable, and
  * they, fail with "expected" failure modes, don't show useless stack traces
  * and just show what the user did wrong
 */
object ErrorTruncationTests extends TestSuite{
  override def utestTruncateLength = 60000
  println("StandaloneTests")
  def checkErrorMessage(file: RelPath, expected: String) = {
    val e = fansi.Str(intercept[ShelloutException]{ exec(file) }.result.err.string).plainText.replace("\r", "").replace("\n", System.lineSeparator())
    assert(e == expected)
  }
  val tests = TestSuite {

    'compileError - checkErrorMessage(
      file = 'errorTruncation/"compileError.sc",
      expected =
        """compileError.sc:1: not found: value doesntexist
          |val res = doesntexist
          |          ^
          |Compilation Failed
          |""".stripMargin.replace("\n", System.lineSeparator())
    )

    'parseError - checkErrorMessage(
      file = 'errorTruncation/"parseError.sc",
      expected =
        """Syntax Error: End:1:1 ..."}\n"
          |}
          |^
          |""".stripMargin.replace("\n", System.lineSeparator())
    )
    val tab = '\t'
    val runtimeErrorResourcePackage =
      "$file.integration.src.test.resources.ammonite.integration.errorTruncation"
    'runtimeError - checkErrorMessage(
      file = 'errorTruncation/"runtimeError.sc",
      expected =
        s"""Exception in thread "main" java.lang.ArithmeticException: / by zero
          |${tab}at $runtimeErrorResourcePackage.runtimeError$$.<init>(runtimeError.sc:1)
          |${tab}at $runtimeErrorResourcePackage.runtimeError$$.<clinit>(runtimeError.sc)
          |${tab}at $runtimeErrorResourcePackage.runtimeError.$$main(runtimeError.sc)
          |""".stripMargin.replace("\n", System.lineSeparator())
    )
  }
}
