package ammonite.repl

import ammonite.repl.frontend._
import ammonite.repl.interp.Interpreter
import utest._


class Checker {
  val interp = new Interpreter(
    (_, _) => (),
    Ref[String]("")
  )

  def session(sess: String): Unit ={
    val margin = sess.lines.filter(_.trim != "").map(_.takeWhile(_ == ' ').length).max
    val steps = sess.replace("\n" + margin, "\n").split("\n\n")
    for(step <- steps){
      val (cmdLines, resultLines) = step.lines.map(_.drop(margin)).partition(_.startsWith("@"))
      val commandText =
        cmdLines.map(_.stripPrefix("@ "))
                .mkString("\n")
      val expected = resultLines.mkString("\n").trim

      if (expected.startsWith("error: ")){
        fail(commandText, _.contains(expected.drop("error: ".length)))
      }else{
        apply(commandText, if (expected == "") null else expected)
      }
    }
  }

  def run(input: String) = {
    print(".")
    val msg = collection.mutable.Buffer.empty[String]
    val processed = interp.processLine(interp.buffered + input, _(_), _.foreach(msg.append(_)))
    val printed = processed.map(_ => msg.mkString)
    interp.handleOutput(processed)
    (processed, printed)
  }
  def apply(input: String,
            expected: String = null) = {
    val (processed, printed) = run(input)

    if (expected != null){
      val expectedRes = Result.Success(expected.trim)
      assert(printed == expectedRes)
    }
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
