package ammonite.shell

import ammonite.ops._
import utest._
import PPrints._
object PPrintTests extends TestSuite{
  def check(lhs: String, rhs: String) = {
    assert(lhs == rhs)
  }
  val pprinter = pprint.PPrinter.BlackWhite.copy(additionalHandlers = Configure.pprintHandlers)
  val tests = TestSuite{

    'paths{
      'pprint{

        check(pprinter.tokenize(root/'hello/'world).mkString, "root/'hello/'world")
        check(pprinter.tokenize('hello/'world).mkString, "'hello/'world")
        check(pprinter.tokenize(empty/'world).mkString, "'world")
        check(pprinter.tokenize(empty/'hello/'world).mkString, "'hello/'world")
        check(pprinter.tokenize(empty/"hello world").mkString, "\"hello world\"")

      }
    }

  }
}
