package ammonite.integration
import ammonite.ops._
import ammonite.util.Util
import utest._
import TestUtils._

/**
  * Mostly already tested in the `ammonite.main` unit tests; one test case is
  * left here just to verify end-to-end correctness
  */
object LineNumberTests extends TestSuite{
  val tests = this{

    def checkErrorMessage(file: RelPath, expected: String): Unit = {
      val e = intercept[ShelloutException]{
        exec(file)
      }.result.err.string
      assert(e.contains(expected))
    }


    'compilationErrorInSecondBlock - checkErrorMessage(
      file = 'lineNumbers/"compilationErrorInSecondBlock.sc",
      expected = Util.normalizeNewlines(
        """compilationErrorInSecondBlock.sc:14: not found: value printnl
          |val res_0 = printnl("OK")
          |            ^""".stripMargin
      )
    )

  }
}
