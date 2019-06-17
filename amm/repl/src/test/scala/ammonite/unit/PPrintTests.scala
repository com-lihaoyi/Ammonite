package ammonite.unit


import utest._

object PPrintTests extends TestSuite{
  def check(lhs: String, rhs: String) = {
    assert(lhs == rhs)
  }
  val pprinter = pprint.PPrinter.BlackWhite.copy(
    additionalHandlers = ammonite.repl.PPrints.replPPrintHandlers
  )
  val tests = Tests{

    test("paths"){
      test("pprint"){

        check(pprinter.tokenize(os.root/'hello/'world).mkString, "root/'hello/'world")
        check(pprinter.tokenize(os.rel/'hello/'world).mkString, "'hello/'world")
        check(pprinter.tokenize(os.rel/'world).mkString, "'world")
        check(pprinter.tokenize(os.rel/'hello/'world).mkString, "'hello/'world")
        check(pprinter.tokenize(os.rel/"hello world").mkString, "\"hello world\"")

      }
    }

  }
}
