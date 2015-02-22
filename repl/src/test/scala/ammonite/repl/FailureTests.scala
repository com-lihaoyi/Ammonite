package ammonite.repl

import utest._

import scala.collection.{immutable => imm}

object FailureTests extends TestSuite{
  val tests = TestSuite{
    val check = new Checker()
    'compileFailure {
      check.fail("doesnt_exist", _.contains("not found: value doesnt_exist"))
      check.fail("java", _.contains("package java is not a value"))
      check.fail("def def", _.contains("identifier expected but 'def' found"))
    }
    'compilerCrash{
      // Make sure compiler crashes provide the appropiate error
      // messaging, and the REPL continues functioning after
      check.fail("trait Bar { super[Object].hashCode }", x =>
        x.contains("Something unexpected went wrong =(") && x.contains("last tree to typer")
      )
      check("1 + 1", "res0: Int = 2")
    }
    'exceptionHandling{
      check.fail("""throw new Exception("lol", new Exception("hoho"))""", x =>
        // It contains the things we want
        x.contains("java.lang.Exception: lol") &&
        x.contains("java.lang.Exception: hoho") &&
        // and none of the stuff we don't want
        x.lines.length == 6 &&
        !x.contains("Something unexpected went wrong =(")
      )
    }
  }
}
