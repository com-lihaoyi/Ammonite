package ammonite.interp

import ammonite.DualTestRepl
import ammonite.util.Util.newLine
import utest._

object PrintTests extends TestSuite{
  val tests = Tests{
    println("PrintTests")
    val check = new DualTestRepl()

    test("simple"){
      for (repl <- check.repls) {
        val t @ (ev, out, res, warn, err, inf) = repl.run("val n = 2", 0)
        val expectedRes = "n: Int = 2"

        assert({ identity(t); ev.isSuccess })
        assert({ identity(t); out.isEmpty })
        assert({ identity(t); res == expectedRes })
      }
    }

    test("out"){
      for (repl <- check.repls) {
        val t @ (ev, out, res, warn, err, inf) = repl.run("show(List(1, 2, 3))", 0)
        val expectedOut = "List(1, 2, 3)" + newLine

        assert({ identity(t); ev.isSuccess })
        assert({ identity(t); out == expectedOut })
        assert({ identity(t); res.isEmpty })
      }
    }

    test("both"){
      for (repl <- check.repls) {
        val t @ (ev, out, res, warn, err, inf) = repl.run("show(List(1, 2, 3)); val n = 3", 0)
        val expectedOut = "List(1, 2, 3)" + newLine
        val expectedRes = "n: Int = 3"

        assert({ identity(t); ev.isSuccess })
        assert({ identity(t); out == expectedOut })
        assert({ identity(t); res == expectedRes })
      }
    }
  }
}
