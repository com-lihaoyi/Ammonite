package ammonite.session

import ammonite.{DualTestRepl, TestUtils}
import utest._

import scala.collection.{immutable => imm}
object BuiltinDepsTests extends TestSuite{

  val tests = Tests{
    println("BuiltinDepsTests")
    val check = new DualTestRepl()
    test("upickleMacros"){
      check.session("""
        @ import upickle.default._

        @ case class Foo(i: Int)

        @ implicit val fooRW: ReadWriter[Foo] = macroRW[Foo]

        @ val res = write(Foo(1))

        @ assert(res == "{\"i\":1}")
      """)
    }
  }
}
