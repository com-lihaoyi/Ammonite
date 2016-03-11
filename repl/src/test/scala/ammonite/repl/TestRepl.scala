package ammonite.repl

import ammonite.repl.frontend._
import ammonite.repl.interp.Interpreter
import utest._
import acyclic.file

import scala.collection.mutable

/**
 * A test REPL which does not read from stdin or stdout files, but instead lets
 * you feed in lines or sessions programmatically and have it execute them.
 */
class TestRepl {
  def predef = ""
  var allOutput = ""


  val tempDir = ammonite.ops.Path(
    java.nio.file.Files.createTempDirectory("ammonite-tester")
  )

  val out = mutable.Buffer.empty[String]
  val warning = mutable.Buffer.empty[String]
  val error = mutable.Buffer.empty[String]
  val printer = Printer(
    out.append(_),
    warning.append(_),
    error.append(_)
  )
  val interp = new Interpreter(
    Ref[String](""),
    Ref(null),
    80,
    80,
    pprint.Config.Defaults.PPrintConfig.copy(height = 15),
    Ref(Colors.BlackWhite),
    printer,
    storage = Ref(Storage(tempDir, None)),
    new History(Vector()),
    predef = predef,
    replArgs = Seq()
  )

  def session(sess: String): Unit = {
    // Remove the margin from the block and break
    // it into blank-line-delimited steps
    val margin = sess.lines.filter(_.trim != "").map(_.takeWhile(_ == ' ').length).min
    // Strip margin & whitespace

    val steps = sess.replace("\n" + margin, "\n").replaceAll(" *\n", "\n").split("\n\n")

    for(step <- steps){
      // Break the step into the command lines, starting with @,
      // and the result lines
      val (cmdLines, resultLines) =
        step.lines
            .map(_.drop(margin))
            .partition(_.startsWith("@"))

      val commandText = cmdLines.map(_.stripPrefix("@ ")).toVector

      // Make sure all non-empty, non-complete command-line-fragments
      // are considered incomplete during the parse
      for (incomplete <- commandText.inits.toSeq.drop(1).dropRight(1)){
        assert(Parsers.split(incomplete.mkString("\n")) == None)
      }

      // Finally, actually run the complete command text through the
      // interpreter and make sure the output is what we expect
      val expected = resultLines.mkString("\n").trim
      allOutput += commandText.map("\n@ " + _).mkString("\n")

      val (processed, out, warning, error) = run(commandText.mkString("\n"))
      interp.handleOutput(processed)

      if (expected.startsWith("error: ")) {
        val strippedExpected = expected.stripPrefix("error: ")
        assert(error.contains(strippedExpected))

      }else if (expected.startsWith("warning: ")){
        val strippedExpected = expected.stripPrefix("warning: ")
        assert(warning.contains(strippedExpected))

      }else if (expected == "") {
        assert(processed.isInstanceOf[Res.Success[_]] || processed.isInstanceOf[Res.Skip.type])

      }else {
        processed match {
          case Res.Success(str) =>
            // Strip trailing whitespace
            def normalize(s: String) = s.lines.map(_.replaceAll(" *$", "")).mkString("\n").trim()
            failLoudly(
              assert{identity(error); identity(warning); normalize(out) == normalize(expected)}
            )

          case Res.Failure(ex, failureMsg) =>
            assert({identity(error); identity(warning); identity(out); identity(expected); false})
          case Res.Exception(ex, failureMsg) =>
            val trace = Repl.showException(ex, "", "", "") + "\n" +  failureMsg
            assert({identity(trace); identity(expected); false})
          case _ => throw new Exception(
            s"Printed $out does not match what was expected: $expected"
          )
        }
      }
    }
  }

  def run(input: String) = {

    out.clear()
    warning.clear()
    error.clear()
    val processed = interp.processLine(
      input,
      Parsers.split(input).get.get.value
    )
    processed match{
      case Res.Failure(ex, s) => printer.error(s)
      case Res.Exception(throwable, msg) =>
        printer.error(Repl.showException(throwable, "", "", ""))
      case _ =>
    }
    interp.handleOutput(processed)
    (processed, out.mkString, warning.mkString("\n"), error.mkString("\n"))
  }


  def fail(input: String,
           failureCheck: String => Boolean = _ => true) = {
    val (processed, out, warning, error) = run(input)

    processed match{
      case Res.Success(v) => assert({identity(v); identity(allOutput); false})
      case Res.Failure(ex, s) =>
        failLoudly(assert(failureCheck(s)))
      case Res.Exception(ex, s) =>
        val msg = Repl.showException(ex, "", "", "") + "\n" + s
        failLoudly(assert(failureCheck(msg)))
      case _ => ???
    }
  }

  def result(input: String, expected: Res[Evaluated]) = {
    val (processed, allOut, warning, error) = run(input)
    assert(processed == expected)
  }
  def failLoudly[T](t: => T) =
    try t
    catch{ case e: utest.framework.AssertionError =>
      println("FAILURE TRACE\n" + allOutput)
      throw e
    }

}
