package ammonite

import java.io.PrintStream

import ammonite.compiler.DefaultCodeWrapper
import ammonite.compiler.iface.CodeWrapper
import ammonite.interp.Interpreter
import ammonite.main.Defaults
import ammonite.repl._
import ammonite.repl.api.{FrontEnd, History, ReplLoad}
import ammonite.runtime.{Frame, Storage}
import ammonite.util.Util.normalizeNewlines
import ammonite.util._
import pprint.{TPrint, TPrintColors}
import utest._

import scala.collection.mutable
import scala.reflect.ClassTag
import ammonite.runtime.ImportHook

/**
 * A test REPL which does not read from stdin or stdout files, but instead lets
 * you feed in lines or sessions programmatically and have it execute them.
 */
class TestRepl { self =>
  var allOutput = ""
  def predef: (String, Option[os.Path]) = ("", None)
  def codeWrapper: CodeWrapper = DefaultCodeWrapper

  val tempDir = os.Path(
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
  val initialClassLoader = Thread.currentThread().getContextClassLoader
  val frames = Ref(List(Frame.createInitial(initialClassLoader)))
  val sess0 = new SessionApiImpl(frames)

  val baseImports = ammonite.main.Defaults.replImports ++ Interpreter.predefImports
  val basePredefs = Seq(
    PredefInfo(Name("testPredef"), predef._1, false, predef._2)
  )
  val customPredefs = Seq()

  val parser = ammonite.compiler.Parsers

  var currentLine = 0
  val interp = try {
    new Interpreter(
      ammonite.compiler.CompilerBuilder,
      parser,
      printer0,
      storage = storage,
      wd = os.pwd,
      colors = Ref(Colors.BlackWhite),
      getFrame = () => frames().head,
      createFrame = () => { val f = sess0.childFrame(frames().head); frames() = f :: frames(); f },
      initialClassLoader = initialClassLoader,
      replCodeWrapper = codeWrapper,
      scriptCodeWrapper = codeWrapper,
      alreadyLoadedDependencies = Defaults.alreadyLoadedDependencies("amm-test-dependencies.txt"),
      importHooks = ImportHook.defaults,
      classPathWhitelist = ammonite.repl.Repl.getClassPathWhitelist(thin = true)
    )

  }catch{ case e: Throwable =>
    println(infoBuffer.mkString)
    println(outString)
    println(resString)
    println(warningBuffer.mkString)
    println(errorBuffer.mkString)
    throw e
  }

  val extraBridges = Seq(
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
      new ReplApiImpl { replApi =>
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
        def fullImports = interp.predefImports ++ imports
        def imports = frames().head.imports
        def usedEarlierDefinitions = frames().head.usedEarlierDefinitions
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

          def exec(file: os.Path): Unit = {
            interp.watch(file)
            apply(normalizeNewlines(os.read(file)))
          }
        }

        override protected[this] def internal0: FullReplAPI.Internal =
          new FullReplAPI.Internal {
            def pprinter = replApi.pprinter
            def colors = replApi.colors
            def replArgs: IndexedSeq[Bind[_]] = replArgs0

            override def print[T: TPrint](
              value: => T,
              ident: String,
              custom: Option[String]
            )(implicit
              tcolors: TPrintColors,
              classTagT: ClassTag[T]
            ): Iterator[String] =
              if (classTagT == scala.reflect.classTag[ammonite.Nope])
                Iterator()
              else
                super.print(value, ident, custom)(TPrint.implicitly[T], tcolors, classTagT)
          }

        def _compilerManager = interp.compilerManager
      }
    ),
    (
      "ammonite.repl.api.FrontEndBridge",
      "frontEnd",
      new FrontEndAPIImpl {
        def parser = self.parser
      }
    )
  )

  for {
    (error, _) <- interp.initializePredef(
      basePredefs, customPredefs, extraBridges, baseImports
    )
  } {
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
        assert(ammonite.compiler.Parsers.split(incomplete.mkString(Util.newLine)).isEmpty)
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
    val splitted = ammonite.compiler.Parsers.split(input) match {
      case None => sys.error(s"No result when splitting input '$input'")
      case Some(Left(error)) => sys.error(s"Error when splitting input '$input': $error")
      case Some(Right(stmts)) => stmts
    }
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
