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
  val tests = TestSuite {

    'compileError{
      val evaled = fansi.Str(
        exec('errorTruncation/"compileError.scala").err.string
      ).plainText
      val expected =
        """compileError.scala:1: not found: value doesntexist
          |val res = doesntexist
          |          ^
          |""".stripMargin

      assert(evaled == expected)
    }
  }
}
