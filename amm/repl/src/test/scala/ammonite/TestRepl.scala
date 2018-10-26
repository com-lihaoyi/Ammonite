package ammonite

import java.io.PrintStream

import ammonite.interp.{Interpreter, Preprocessor}
import ammonite.main.Defaults
import ammonite.ops.{Path, read}
import ammonite.repl._
import ammonite.runtime.{Frame, History, Storage}
import ammonite.util.Util.normalizeNewlines
import ammonite.util._
import utest._

import scala.collection.mutable

/**
 * A test REPL which does not read from stdin or stdout files, but instead lets
 * you feed in lines or sessions programmatically and have it execute them.
 */
class TestRepl {
  var allOutput = ""
  def predef: (String, Option[ammonite.ops.Path]) = ("", None)
  def codeWrapper: Preprocessor.CodeWrapper = Preprocessor.CodeWrapper

  val tempDir = ammonite.ops.Path(
    java.nio.file.Files.createTempDirectory("ammonite-tester")
  )


  import java.io.ByteArrayOutputStream
  import java.io.PrintStream

  val outBytes = new ByteArrayOutputStream
  val errBytes = new ByteArrayOutputStream
  val resBytes = new ByteArrayOutputStream
  def outString = new String(outBytes.toByteArray)
  def resString = new String(resBytes.toByteArray)

  val warningBuffer = mutable.Buffer.empty[String]
  val errorBuffer = mutable.Buffer.empty[String]
  val infoBuffer = mutable.Buffer.empty[String]
  val printer0 = Printer(
    new PrintStream(outBytes),
    new PrintStream(errBytes),
    new PrintStream(resBytes),
    x => warningBuffer.append(x + Util.newLine),
    x => errorBuffer.append(x + Util.newLine),
    x => infoBuffer.append(x + Util.newLine)
  )
  val storage = new Storage.Folder(tempDir)
  val frames = Ref(List(Frame.createInitial()))
  val sess0 = new SessionApiImpl(frames)

  var currentLine = 0
  val interp: Interpreter = try {
    new Interpreter(
      printer0,
      storage = storage,
      wd = ammonite.ops.pwd,
      basePredefs = Seq(
        PredefInfo(
          Name("defaultPredef"),
          ammonite.main.Defaults.replPredef + ammonite.main.Defaults.predefString,
          true,
          None
        ),
        PredefInfo(Name("testPredef"), predef._1, false, predef._2)
      ),
      customPredefs = Seq(),
      extraBridges = Seq(
        (
          "ammonite.TestReplBridge",
          "test",
          new TestReplApi {
            def message = "ba"
          }
        ),
        (
          "ammonite.repl.ReplBridge",
          "repl",
          new ReplApiImpl {
            def replArgs0 = Vector.empty[Bind[_]]
            def printer = printer0

            def sess = sess0
            val prompt = Ref("@")
            val frontEnd = Ref[FrontEnd](null)
            def lastException: Throwable = null
            def fullHistory = storage.fullHistory()
            def history = new History(Vector())
            val colors = Ref(Colors.BlackWhite)
            def newCompiler() = interp.compilerManager.init(force = true)
            def compiler = interp.compilerManager.compiler.compiler
            def fullImports = interp.predefImports ++ imports
            def imports = interp.frameImports
            def usedEarlierDefinitions = interp.frameUsedEarlierDefinitions
            def width = 80
            def height = 80

            object load extends ReplLoad with (String => Unit){

              def apply(line: String) = {
                interp.processExec(line, currentLine, () => currentLine += 1) match{
                  case Res.Failure(s) => throw new CompilationError(s)
                  case Res.Exception(t, s) => throw t
                  case _ =>
                }
              }

              def exec(file: Path): Unit = {
                interp.watch(file)
                apply(normalizeNewlines(read(file)))
              }
            }
          }
        )
      ),
      colors = Ref(Colors.BlackWhite),
      getFrame = () => frames().head,
      createFrame = () => { val f = sess0.childFrame(frames().head); frames() = f :: frames(); f },
      replCodeWrapper = codeWrapper,
      scriptCodeWrapper = codeWrapper,
      alreadyLoadedDependencies = Defaults.alreadyLoadedDependencies("amm-test-dependencies.txt")
    )

  }catch{ case e: Throwable =>
    println(infoBuffer.mkString)
    println(outString)
    println(resString)
    println(warningBuffer.mkString)
    println(errorBuffer.mkString)
    throw e
  }


  for ((error, _) <- interp.initializePredef()) {
    val (msgOpt, causeOpt) = error match {
      case r: Res.Exception => (Some(r.msg), Some(r.t))
      case r: Res.Failure => (Some(r.msg), None)
      case _ => (None, None)
    }

    println(infoBuffer.mkString)
    println(outString)
    println(resString)
    println(warningBuffer.mkString)
    println(errorBuffer.mkString)
    throw new Exception(
      s"Error during predef initialization${msgOpt.fold("")(": " + _)}",
      causeOpt.orNull
    )
  }



  def session(sess: String): Unit = {
    // Remove the margin from the block and break
    // it into blank-line-delimited steps
    val margin = Predef.augmentString(sess)
      .lines
      .filter(_.trim != "")
      .map(_.takeWhile(_ == ' ').length)
      .min
    // Strip margin & whitespace

    val steps = sess.replace(
      Util.newLine + margin, Util.newLine
    ).replaceAll(" *\n", "\n").split("\n\n")

    for((step, index) <- steps.zipWithIndex){
      // Break the step into the command lines, starting with @,
      // and the result lines
      val (cmdLines, resultLines) =
        Predef.augmentString(step).lines.toArray.map(_.drop(margin)).partition(_.startsWith("@"))

      val commandText = cmdLines.map(_.stripPrefix("@ ")).toVector

      println(cmdLines.mkString(Util.newLine))
      // Make sure all non-empty, non-complete command-line-fragments
      // are considered incomplete during the parse
      //
      // ...except for the empty 0-line fragment, and the entire fragment,
      // both of which are complete.
      for (incomplete <- commandText.inits.toSeq.drop(1).dropRight(1)){
        assert(ammonite.interp.Parsers.split(incomplete.mkString(Util.newLine)).isEmpty)
      }

      // Finally, actually run the complete command text through the
      // interpreter and make sure the output is what we expect
      val expected = resultLines.mkString(Util.newLine).trim
      allOutput += commandText.map(Util.newLine + "@ " + _).mkString(Util.newLine)

      val (processed, out, res, warning, error, info) =
        run(commandText.mkString(Util.newLine), currentLine)

      val allOut = out + res

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
              identity(res)
              identity(info)
              false
            }
        }

      }else {
        processed match {
          case Res.Success(str) =>
            // Strip trailing whitespace
            def normalize(s: String) =
              Predef.augmentString(s)
                .lines
                .map(_.replaceAll(" *$", ""))
                .mkString(Util.newLine)
                .trim()

            val expected0 = normalize(expected)

            if (expected0.endsWith(" = ?")) {
              val expectedStart = expected0.stripSuffix("?")
              failLoudly(
                assert{
                  identity(error)
                  identity(warning)
                  identity(info)
                  normalize(allOut).take(expectedStart.length) == expectedStart
                }
              )
            } else
              failLoudly(
                assert{
                  identity(error)
                  identity(warning)
                  identity(info)
                  normalize(allOut) == expected0
                }
              )

          case Res.Failure(failureMsg) =>
            assert{
              identity(error)
              identity(warning)
              identity(out)
              identity(res)
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
            s"Printed $allOut does not match what was expected: $expected"
          )
        }
      }
    }
  }



  def run(input: String, index: Int) = {

    outBytes.reset()
    resBytes.reset()
    warningBuffer.clear()
    errorBuffer.clear()
    infoBuffer.clear()
    val splitted = ammonite.interp.Parsers.split(input).get.get.value
    val processed = interp.processLine(
      input,
      splitted,
      index,
      false,
      () => currentLine += 1
    )
    processed match{
      case Res.Failure(s) => printer0.error(s)
      case Res.Exception(throwable, msg) =>
        printer0.error(
          Repl.showException(throwable, fansi.Attrs.Empty, fansi.Attrs.Empty, fansi.Attrs.Empty)
        )

      case _ =>
    }
    Repl.handleOutput(interp, processed)
    (
      processed,
      outString,
      resString,
      warningBuffer.mkString,
      errorBuffer.mkString,
      infoBuffer.mkString
    )
  }


  def fail(input: String,
           failureCheck: String => Boolean = _ => true) = {
    val (processed, out, _, warning, error, info) = run(input, 0)

    processed match{
      case Res.Success(v) => assert({identity(v); identity(allOutput); false})
      case Res.Failure(s) =>
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
    val (processed, allOut, _, warning, error, info) = run(input, 0)
    assert(processed == expected)
  }
  def failLoudly[T](t: => T) =
    try t
    catch{ case e: utest.AssertionError =>
      println("FAILURE TRACE" + Util.newLine + allOutput)
      throw e
    }

}
