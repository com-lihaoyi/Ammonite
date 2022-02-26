package ammonite.integration
import ammonite.util.Util
import utest._
import TestUtils._

/**
  * Mostly already tested in the `ammonite.main` unit tests; one test case is
  * left here just to verify end-to-end correctness
  */
object LineNumberTests extends TestSuite{
  val tests = this{

    def checkErrorMessage(file: os.RelPath, expected: String): Unit = {
      val e = intercept[os.SubprocessException]{
        exec(file)
      }.result.err.string
      assert(e.contains(expected))
    }


    test("compilationErrorInSecondBlock") - checkErrorMessage(
      file = os.rel/'lineNumbers/"compilationErrorInSecondBlock.sc",
      expected = Util.normalizeNewlines(
        if (isScala2)
          """compilationErrorInSecondBlock.sc:14: not found: value printnl
            |val res_0 = printnl("OK")
            |            ^""".stripMargin
        else
          """|   |val res_0 = printnl("OK")
             |   |            ^^^^^^^
             |   |            Not found: printnl
             |Compilation Failed""".stripMargin
      )
    )

  }
}
