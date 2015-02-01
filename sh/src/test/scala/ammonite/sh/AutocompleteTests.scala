package ammonite.sh

import utest._

import scala.collection.{immutable => imm}

object AutocompleteTests extends TestSuite{

  val tests = TestSuite{
    val check = new Checker()
    'simpleExpressions{
      check("1 + \"2\"", "res0: String = \"12\"")

    }
  }
}

