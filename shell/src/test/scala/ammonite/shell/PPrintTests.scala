package ammonite.shell

import ammonite.ops._
import utest._
import PPrints._
object PPrintTests extends TestSuite {
  def check(lhs: String, rhs: String) = {
    assert(lhs == rhs)
  }
  val tests = TestSuite {

    'paths {
      'pprint {
        import pprint.Config.Defaults._
        check(pprint.tokenize(root / 'hello / 'world).mkString,
              "root/'hello/'world")
        check(pprint.tokenize('hello / 'world).mkString, "'hello/'world")
        check(pprint.tokenize(empty / 'world).mkString, "'world")
        check(pprint.tokenize(empty / 'hello / 'world).mkString,
              "'hello/'world")
        check(pprint.tokenize(empty / "hello world").mkString,
              "\"hello world\"")

      }
    }

  }
}
