package ammonite.interp

import ammonite.DualTestRepl
import utest._

import scala.util.Success

object InfoTests extends TestSuite {

  class InfoChecker {
    val check = new DualTestRepl
    def apply(caretCode: String, expected: String): Unit = {
      val cursor = caretCode.indexOf("<caret>")
      val buf = caretCode.replace("<caret>", "")

      for (interp <- check.interps) {
        val info = interp.compilerManager.info(
          cursor,
          interp.frameImports.toString(),
          buf
        )
        assert(info == Success(expected))
      }
    }
  }

  def checking[T](f: InfoChecker => T) = {
    val c = new InfoChecker
    val res = f(c)
    c.check.interps.foreach(_.compilerManager.shutdownPressy())
    res
  }

  val tests = Tests {
    'simple - {
      checking { checker =>
        // don't know if the added '=> ' is legit here...
        checker("val n = 2; n<caret> + 1", "=> Int")
      }
    }
  }

}
