package ammonite.repl

import utest._

import scala.collection.{immutable => imm}

object AdvancedTests extends TestSuite{
  val tests = TestSuite{
    val check = new Checker()
    'loadIvy{
      check.fail("import scalatags.Text.all._", _.contains("not found: value scalatags"))
      check.fail("import scalatags.Text.all._", _.contains("not found: value scalatags"))
    }


  }
}
