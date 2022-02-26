package ammonite.session

import ammonite.{DualTestRepl, TestUtils}
import utest._

import scala.collection.{immutable => imm}
object FailureTests extends TestSuite{
  val tests = Tests{
    println("FailureTests")
    val check = new DualTestRepl()
    test("compileFailure"){
      if (check.scala2)
        check.session(s"""
          @ doesnt_exist
          error: not found: value doesnt_exist

          @ java
          error: java is not a value

          @ 1 + vale
          error: not found: value vale
          val res0 = 1 + vale
                         ^
          Compilation Failed

          @ val x = 1 + vale
          error: not found: value vale
          val x = 1 + vale
                      ^
          Compilation Failed
        """)
      else
        check.session(s"""
          @ doesnt_exist
          error: Not found: doesnt_exist

          @ java
          error: java is not a value

          @ 1 + vale
          error: Not found: vale
          Compilation Failed

          @ val x = 1 + vale
          error: Not found: vale
          Compilation Failed
        """)
    }
    test("compilerCrash"){
      // Make sure compiler crashes provide the appropriate error
      // messaging, and the REPL continues functioning after
      if (TestUtils.scala2_11) check.session("""
        @ val x = 1
        x: Int = 1

        @ /* trigger compiler crash */ trait Bar { super[Object].hashCode }
        error: java.lang.AssertionError: assertion failed

        @ 1 + x
        res1: Int = 2
      """)
    }
    test("ivyFail"){
      check.session("""
        @ import $ivy.`com.lihaoyi::upickle-doest-exist:0.1.0`
        error: Failed to resolve ivy dependencies
      """)
    }

    test("exceptionHandling"){
      check.fail("""throw new Exception("lol", new Exception("hoho"))""", x =>
        // It contains the things we want
        x.contains("java.lang.Exception: lol") &&
        x.contains("java.lang.Exception: hoho") &&
        // and none of the stuff we don't want
        !x.contains("evaluatorRunPrinter") &&
        !x.contains("Something unexpected went wrong =(")
      )
    }
  }
}
