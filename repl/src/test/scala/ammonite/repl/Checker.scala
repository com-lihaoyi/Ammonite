package ammonite.repl

import ammonite.repl.frontend._
import ammonite.repl.interp.Interpreter
import utest._


class Checker {
  var allOutput = ""
  val interp = new Interpreter(
    (_, _) => (),
    Ref[String](""),
    stdout = allOutput += _,
    initialHistory = Nil
  )

  def session(sess: String): Unit ={
//    println("SESSION")
//    println(sess)
    val margin = sess.lines.filter(_.trim != "").map(_.takeWhile(_ == ' ').length).min
    val steps = sess.replace("\n" + margin, "\n").split("\n\n")
    for(step <- steps){

      val (cmdLines, resultLines) = step.lines.map(_.drop(margin)).partition(_.startsWith("@"))
      val commandText = cmdLines.map(_.stripPrefix("@ ")).toVector

      val expected = resultLines.mkString("\n").trim
      for(line <- commandText.init) {
        allOutput += "\n@ " + line
        val (processed, printed) = run(line)
        if (!line.startsWith("//")) {
          failLoudly(assert(processed.isInstanceOf[Res.Buffer]))
        }

        interp.handleOutput(processed)
      }
      if (expected.startsWith("error: ")){
        fail(commandText.last, _.contains(expected.drop("error: ".length)))
      }else{
        apply(commandText.last, if (expected == "") null else expected)
      }
    }
  }

  def run(input: String) = {
//    println("RUNNING")
//    println(input)
//    print(".")
    val msg = collection.mutable.Buffer.empty[String]
    val processed = interp.processLine(interp.buffered + input, _(_), _.foreach(msg.append(_)))
    val printed = processed.map(_ => msg.mkString)
    if (!printed.isInstanceOf[Res.Buffer])
      allOutput += "\n" + printed
    interp.handleOutput(processed)
    (processed, printed)
  }

  def apply(input: String,
            expected: String = null) = {
    val (processed, printed) = run(input)
    if (expected != null){
      val expectedRes = Res.Success(expected.trim)
      failLoudly(assert(printed == expectedRes))
    }
  }

  def fail(input: String,
           failureCheck: String => Boolean = _ => true) = {
    val (processed, printed) = run(input)

    printed match{
      case Res.Success(v) => assert({v; allOutput; false})
      case Res.Failure(s) =>

        failLoudly(assert(failureCheck(s)))
    }
  }

  def result(input: String, expected: Res[Evaluated]) = {
    val (processed, printed) = run(input)
    assert(processed == expected)
  }
  def failLoudly[T](t: => T) = try{
      t
  } catch{ case e: utest.AssertionError =>
    println("FAILURE TRACE\n" + allOutput)
    throw e
  }

}
