package ammonite

import ammonite.frontend.{ReplApiImpl, SessionApiImpl}
import ammonite.interp.{APIHolder, History, Interpreter, Storage}
import ammonite.main.Repl
import ammonite.util._
import utest.asserts._

import scala.collection.mutable

/**
 * A test REPL which does not read from stdin or stdout files, but instead lets
 * you feed in lines or sessions programmatically and have it execute them.
 */
class TestRepl {
  var allOutput = ""
  def predef = ""

  val tempDir = ammonite.ops.Path(
    java.nio.file.Files.createTempDirectory("ammonite-tester")
  )

  val outBuffer = mutable.Buffer.empty[String]
  val warningBuffer = mutable.Buffer.empty[String]
  val errorBuffer = mutable.Buffer.empty[String]
  val infoBuffer = mutable.Buffer.empty[String]
  val printer = Printer(
    outBuffer.append(_),
    warningBuffer.append(_),
    errorBuffer.append(_),
    infoBuffer.append(_)
  )
  val interp = try {

    val i = new Interpreter(
      printer,
      storage = new Storage.Folder(tempDir),
      predef = ammonite.main.Defaults.predefString + Util.newLine + predef,
      wd = ammonite.ops.cwd,
      customPredefs = Seq()
    )
    APIHolder.initBridge(
      i.evalClassloader,
      "ammonite.frontend.ReplBridge",
      new ReplApiImpl(
        i,
        80,
        80,
        Colors.BlackWhite,
        "@",
        Ref(null),
        new History(Vector()),
        new SessionApiImpl(i.eval)
      )
    )
    i.init()
    i
  }catch{ case e =>
    println(infoBuffer.mkString)
    println(outBuffer.mkString)
    println(warningBuffer.mkString)
    println(errorBuffer.mkString)
    throw e
  }

  def session(sess: String): Unit = {
    // Remove the margin from the block and break
    // it into blank-line-delimited steps
    val margin = sess.lines.filter(_.trim != "").map(_.takeWhile(_ == ' ').length).min
    // Strip margin & whitespace

    val steps = sess.replace(
      Util.newLine + margin, Util.newLine
    ).replaceAll(" *\n", "\n").split("\n\n")

    for((step, index) <- steps.zipWithIndex){
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
        assert(ammonite.interp.Parsers.split(incomplete.mkString(Util.newLine)) == None)
      }

      // Finally, actually run the complete command text through the
      // interpreter and make sure the output is what we expect
      val expected = resultLines.mkString(Util.newLine).trim
      allOutput += commandText.map(Util.newLine + "@ " + _).mkString(Util.newLine)

      val (processed, out, warning, error, info) = run(commandText.mkString(Util.newLine), index)
      interp.handleOutput(processed)

      if (expected.startsWith("error: ")) {
        val strippedExpected = expected.stripPrefix("error: ")
        assert(error.contains(strippedExpected))

      }else if (expected.startsWith("warning: ")){
        val strippedExpected = expected.stripPrefix("warning: ")
        assert(warning.contains(strippedExpected))

      }else if (expected.startsWith("info: ")){
        val strippedExpected = expected.stripPrefix("info: ")
        assert(info.contains(strippedExpected))

      }else if (expected == "") {
        processed match{
          case Res.Success(_) => // do nothing
          case Res.Skip => // do nothing
          case _: Res.Failing =>
            assert{
              identity(error)
              identity(warning)
              identity(out)
              identity(info)
              false
            }
        }

      }else {
        processed match {
          case Res.Success(str) =>
            // Strip trailing whitespace
            def normalize(s: String) =
              s.lines
                .map(_.replaceAll(" *$", ""))
                .mkString(Util.newLine)
                .trim()
            failLoudly(
              assert{
                identity(error)
                identity(warning)
                identity(info)
                normalize(out) == normalize(expected)
              }
            )

          case Res.Failure(ex, failureMsg) =>
            assert{
              identity(error)
              identity(warning)
              identity(out)
              identity(info)
              identity(expected)
              false
            }
          case Res.Exception(ex, failureMsg) =>
            val trace = Repl.showException(
              ex, fansi.Attrs.Empty, fansi.Attrs.Empty, fansi.Attrs.Empty
            ) + Util.newLine +  failureMsg
            assert({identity(trace); identity(expected); false})
          case _ => throw new Exception(
            s"Printed $out does not match what was expected: $expected"
          )
        }
      }
    }
  }



  def run(input: String, index: Int) = {

    outBuffer.clear()
    warningBuffer.clear()
    errorBuffer.clear()
    infoBuffer.clear()
    val processed = interp.processLine(
      input,
      ammonite.interp.Parsers.split(input).get.get.value,
      s"Main$index.sc"
    )
    processed match{
      case Res.Failure(ex, s) => printer.error(s)
      case Res.Exception(throwable, msg) =>
        printer.error(
          Repl.showException(throwable, fansi.Attrs.Empty, fansi.Attrs.Empty, fansi.Attrs.Empty)
        )

      case _ =>
    }
    interp.handleOutput(processed)
    (
      processed,
      outBuffer.mkString,
      warningBuffer.mkString(Util.newLine),
      errorBuffer.mkString(Util.newLine),
      infoBuffer.mkString(Util.newLine)
    )
  }


  def fail(input: String,
           failureCheck: String => Boolean = _ => true) = {
    val (processed, out, warning, error, info) = run(input, 0)

    processed match{
      case Res.Success(v) => assert({identity(v); identity(allOutput); false})
      case Res.Failure(ex, s) =>
        failLoudly(assert(failureCheck(s)))
      case Res.Exception(ex, s) =>
        val msg = Repl.showException(
          ex, fansi.Attrs.Empty, fansi.Attrs.Empty, fansi.Attrs.Empty
        ) + Util.newLine + s
        failLoudly(assert(failureCheck(msg)))
      case _ => ???
    }
  }


  def result(input: String, expected: Res[Evaluated]) = {
    val (processed, allOut, warning, error, info) = run(input, 0)
    assert(processed == expected)
  }
  def failLoudly[T](t: => T) =
    try t
    catch{ case e: utest.AssertionError =>
      println("FAILURE TRACE" + Util.newLine + allOutput)
      throw e
    }

}
