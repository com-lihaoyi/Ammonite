package ammonite

import ammonite.runtime.Storage
import ammonite.repl.Repl
import ammonite.kernel.ReplKernel
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
  val printer = new PrinterX(
    outBuffer.append(_),
    warningBuffer.append(_),
    errorBuffer.append(_),
    infoBuffer.append(_)
  )

  val storage = new Storage.Folder(tempDir)

  val predefs = Seq(
    Name("pprintPredef") -> Repl.pprintPredef,
    Name("defaultPredef") -> ammonite.main.Defaults.predefString,
    Name("testPredef") -> predef
  )

  val kernel = new ReplKernel(printer, storage, predefs, ammonite.ops.pwd)

  def session(sess: String): Unit = {
    // Remove the margin from the block and break
    // it into blank-line-delimited steps
    val margin =
      sess.lines.filter(_.trim != "").map(_.takeWhile(_ == ' ').length).min
    // Strip margin & whitespace

    val steps = sess
      .replace(
        Util.newLine + margin,
        Util.newLine
      )
      .replaceAll(" *\n", "\n")
      .split("\n\n")

    for ((step, index) <- steps.zipWithIndex) {
      // Break the step into the command lines, starting with @,
      // and the result lines
      val (cmdLines, resultLines) =
        step.lines.map(_.drop(margin)).partition(_.startsWith("@"))

      val commandText = cmdLines.map(_.stripPrefix("@ ")).toVector

      // Make sure all non-empty, non-complete command-line-fragments
      // are considered incomplete during the parse
      for (incomplete <- commandText.inits.toSeq.drop(1).dropRight(1)) {
        assert(ammonite.runtime.Parsers.split(incomplete.mkString(Util.newLine)) == None)
      }

      // Finally, actually run the complete command text through the
      // interpreter and make sure the output is what we expect
      val expected = resultLines.mkString(Util.newLine).trim
      allOutput += commandText.map(Util.newLine + "@ " + _).mkString(Util.newLine)

      val (processed, out, warning, error, info) = run(commandText.mkString(Util.newLine))
      kernel.interp.handleOutput(processed)

      if (expected.startsWith("error: ")) {
        val strippedExpected = expected.stripPrefix("error: ")
        assert(error.contains(strippedExpected))

      } else if (expected.startsWith("warning: ")) {
        val strippedExpected = expected.stripPrefix("warning: ")
        assert(warning.contains(strippedExpected))

      } else if (expected.startsWith("info: ")) {
        val strippedExpected = expected.stripPrefix("info: ")
        assert(info.contains(strippedExpected))

      } else if (expected == "") {
        processed match {
          case Res.Success(_) => // do nothing
          case Res.Skip => // do nothing
          case _: Res.Failing =>
            assert {
              identity(error)
              identity(warning)
              identity(out)
              identity(info)
              false
            }
        }

      } else {
        processed match {
          case Res.Success(str) =>
            // Strip trailing whitespace
            def normalize(s: String) =
              s.lines.map(_.replaceAll(" *$", "")).mkString(Util.newLine).trim()
            failLoudly(
              assert {
                identity(error)
                identity(warning)
                identity(info)
                normalize(out) == normalize(expected)
              }
            )

          case Res.Failure(ex, failureMsg) =>
            assert {
              identity(error)
              identity(warning)
              identity(out)
              identity(info)
              identity(expected)
              false
            }
          case Res.Exception(ex, failureMsg) =>
            val trace = Repl.showException(
                ex,
                fansi.Attrs.Empty,
                fansi.Attrs.Empty,
                fansi.Attrs.Empty
              ) + Util.newLine + failureMsg
            assert({ identity(trace); identity(expected); false })
          case _ =>
            throw new Exception(
              s"Printed $out does not match what was expected: $expected"
            )
        }
      }
    }
  }

  def run(input: String) = {

    outBuffer.clear()
    warningBuffer.clear()
    errorBuffer.clear()
    infoBuffer.clear()
    val processed = kernel.process(input)
    processed match {
      case Res.Failure(ex, s) => printer.error(s)
      case Res.Exception(throwable, msg) =>
        printer.error(
          Repl.showException(throwable, fansi.Attrs.Empty, fansi.Attrs.Empty, fansi.Attrs.Empty)
        )
      case _ =>
    }
    (
      processed,
      outBuffer.mkString,
      warningBuffer.mkString(Util.newLine),
      errorBuffer.mkString(Util.newLine),
      infoBuffer.mkString(Util.newLine)
    )
  }

  def fail(input: String, failureCheck: String => Boolean = _ => true) = {
    val (processed, _, _, _, _) = run(input)

    processed match {
      case Res.Success(v) =>
        assert({ identity(v); identity(allOutput); false })
      case Res.Failure(ex, s) =>
        failLoudly(assert(failureCheck(s)))
      case Res.Exception(ex, s) =>
        val msg = Repl.showException(
            ex,
            fansi.Attrs.Empty,
            fansi.Attrs.Empty,
            fansi.Attrs.Empty
          ) + Util.newLine + s
        failLoudly(assert(failureCheck(msg)))
      case _ => ???
    }
  }

  def result(input: String, expected: Res[Evaluated]) = {
    val (processed, _, _, _, _) = run(input)
    assert(processed == expected)
  }
  def failLoudly[T](t: => T) =
    try t
    catch {
      case e: utest.AssertionError =>
        println("FAILURE TRACE" + Util.newLine + allOutput)
        throw e
    }

}
