package ammonite.repl

import utest._

import scala.collection.{immutable => imm}

object FailureTests extends TestSuite{
  val tests = TestSuite{
    val check = new Checker()
    check.fail("doesnt_exist", _.contains("not found: value doesnt_exist"))
    check.fail("java", _.contains("package java is not a value"))
    check.fail("def def", _.contains("identifier expected but 'def' found"))

  }
}
