package ammonite.repl

import ammonite.repl.frontend._
import ammonite.repl.interp.Interpreter
import utest._


class Checker {
  val interp = new Interpreter(
    (_, _) => (),
    Ref[String]("")
  )



  def run(input: String) = {
    print(".")
    val msg = collection.mutable.Buffer.empty[String]
    val processed = interp.processLine(interp.buffered + input, _ => (), _.foreach(msg.append(_)))
    val printed = processed.map(_ => msg.mkString)
    interp.handleOutput(processed)
    (processed, printed)
  }
  def apply(input: String,
            expected: String = null) = {
    val (processed, printed) = run(input)

    if (expected != null)
      assert(printed == Result.Success(expected.trim))
  }
  def fail(input: String,
           failureCheck: String => Boolean = _ => true) = {
    val (processed, printed) = run(input)

    printed match{
      case Result.Success(v) => assert({v; false})
      case Result.Failure(s) => assert(failureCheck(s))
    }
  }
  def result(input: String, expected: Result[Evaluated]) = {
    val (processed, printed) = run(input)
    assert(processed == expected)
  }
}
