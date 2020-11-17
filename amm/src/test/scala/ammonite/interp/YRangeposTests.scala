package ammonite.interp

import ammonite.TestUtils._

import ammonite.runtime.Storage
import ammonite.main._
import utest._

import scala.tools.nsc.Global

object YRangeposTests extends TestSuite {
  val tests = Tests {
    println("YRangeposTests")

    def checkErrorMessage(file: os.RelPath, expected: String): Unit = {
      val e = new InProcessMainMethodRunner(file, Nil, Nil)

      assert(e.err.contains(expected))
    }

    val scriptFolderPath =
      os.pwd / 'amm / 'src / 'test / 'resources / 'scriptCompilerSettings

    test("Yrangepos"){
      // This tests shows that enabling Yrangepos does not mess with ammonite's
      // behaviour. The compiler not crashing is the test itself.
      val storage = Storage.InMemory()
      val interp = createTestInterp(
        storage,
        predefImports = Interpreter.predefImports
      )
      val res = Scripts.runScript(os.pwd, scriptFolderPath / "yRangepos.sc", interp)
      assert(res.isSuccess)
    }

    test("YrangeposError"){
      // This tests shows that enabling Yrangepos does not mess with ammonite's
      // behaviour, by checking that the line at which the error is found matches
      // the expected one in the file
      val expectedErrorMessage = "yRangeposError.sc:9: type mismatch;"
      checkErrorMessage(os.rel / 'scriptCompilerSettings / "yRangeposError.sc",
        expectedErrorMessage)
    }
  }
}
