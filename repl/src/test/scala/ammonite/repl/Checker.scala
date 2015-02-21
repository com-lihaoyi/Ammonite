package ammonite.repl

import ammonite.repl.frontend._
import ammonite.repl.interp.Interpreter
import utest._


class Checker {
  val interp = new Interpreter(
    _ => (),
    Ref[String]("")
  )

  def handleResult(res: Result[Evaluated]) = {
    interp.eval.update(res.asInstanceOf[Result.Success[Evaluated]].s.imports)
  }

  def apply(input: String,
            expected: String = null) = {
    print(".")
    val processed = interp.processLine(input, _ =>())
    val printed = processed.map(_.msg.mkString)

    if (expected != null)
      assert(printed == Result.Success(expected.trim))

    handleResult(processed)
  }
  def fail(input: String,
           failureCheck: String => Boolean = _ => true) = {
    print(".")
    val processed = interp.processLine(input, _ => ())
    val printed = processed.map(_.msg.mkString)

    printed match{
      case Result.Success(v) => assert({v; false})
      case Result.Failure(s) => assert(failureCheck(s))
    }
  }
}
