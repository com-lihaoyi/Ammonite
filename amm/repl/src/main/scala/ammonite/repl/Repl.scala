package ammonite.repl

import java.io.{InputStream, InputStreamReader, OutputStream}

import ammonite.runtime._
import ammonite.terminal.Filter
import ammonite.util.Util.{newLine, normalizeNewlines}
import ammonite.util._
import ammonite.interp.{Interpreter, Parsers, Preprocessor}


import scala.annotation.tailrec

class Repl(input: InputStream,
           output: OutputStream,
           error: OutputStream,
           storage: Storage,
           basePredefs: Seq[PredefInfo],
           customPredefs: Seq[PredefInfo],
           wd: os.Path,
           welcomeBanner: Option[String],
           replArgs: IndexedSeq[Bind[_]] = Vector.empty,
           initialColors: Colors = Colors.Default,
           replCodeWrapper: Preprocessor.CodeWrapper,
           scriptCodeWrapper: Preprocessor.CodeWrapper,
           alreadyLoadedDependencies: Seq[coursier.Dependency]) { repl =>

  val prompt = Ref("@ ")

  val frontEnd = Ref[FrontEnd](
    if (scala.util.Properties.isWin)
      ammonite.repl.FrontEnd.JLineWindows
    else
      AmmoniteFrontEnd(Filter.empty)
  )

  var lastException: Throwable = null

  var history = new History(Vector())

  val (colors, printer) =
    Interpreter.initPrinters(initialColors, output, error, true)

  val argString = replArgs.zipWithIndex.map{ case (b, idx) =>
    s"""
    val ${b.name} =
      ammonite.repl.ReplBridge.value.Internal.replArgs($idx).value.asInstanceOf[${b.typeTag.tpe}]
    """
  }.mkString(newLine)

  val frames = Ref(List(Frame.createInitial()))

  /**
    * The current line number of the REPL, used to make sure every snippet
    * evaluated can have a distinct name that doesn't collide.
    */
  var currentLine = 0


  val sess0 = new SessionApiImpl(frames)

  def imports = frames().head.imports
  def fullImports = interp.predefImports ++ imports

  def usedEarlierDefinitions = frames().head.usedEarlierDefinitions

  val interp: Interpreter = new Interpreter(
    printer,
    storage,
    basePredefs,
    customPredefs,
    Seq((
      "ammonite.repl.ReplBridge",
      "repl",
      new ReplApiImpl {
        def replArgs0 = repl.replArgs
        def printer = repl.printer
        val colors = repl.colors
        def sess = repl.sess0
        val prompt = repl.prompt
        val frontEnd = repl.frontEnd

        def lastException = repl.lastException
        def fullHistory = storage.fullHistory()
        def history = repl.history
        def newCompiler() = interp.compilerManager.init(force = true)
        def compiler = interp.compilerManager.compiler.compiler
        def interactiveCompiler = interp.compilerManager.pressy.compiler
        def fullImports = repl.fullImports
        def imports = repl.imports
        def usedEarlierDefinitions = repl.usedEarlierDefinitions
        def width = frontEnd().width
        def height = frontEnd().height

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
      }
    )),
    wd,
    colors,
    verboseOutput = true,
    getFrame = () => frames().head,
    createFrame = () => { val f = sess0.childFrame(frames().head); frames() = f :: frames(); f },
    replCodeWrapper = replCodeWrapper,
    scriptCodeWrapper = scriptCodeWrapper,
    alreadyLoadedDependencies = alreadyLoadedDependencies
  )

  def initializePredef() = interp.initializePredef()

  def warmup() = {
    // An arbitrary input, randomized to make sure it doesn't get cached or
    // anything anywhere (though it shouldn't since it's processed as a line).
    //
    // Should exercise the main code paths that the Ammonite REPL uses, and
    // can be run asynchronously while the user is typing their first command
    // to make sure their command reaches an already-warm command when submitted.
    //
    // Otherwise, this isn't a particularly complex chunk of code and shouldn't
    // make the minimum first-compilation time significantly longer than just
    // running the user code directly. Could be made longer to better warm more
    // code paths, but then the fixed overhead gets larger so not really worth it
    val code = s"""val array = Seq.tabulate(10)(_*2).toArray.max"""
    val stmts = Parsers.split(code).get.get.value
    interp.processLine(code, stmts, 9999999, silent = true, () => () /*donothing*/)
  }


  sess0.save()
  interp.createFrame()

  val reader = new InputStreamReader(input)

  def action() = for{
    _ <- Catching {
      case Ex(e: ThreadDeath) =>
        Thread.interrupted()
        Res.Failure("Interrupted!")

      case ex => Res.Exception(ex, "")
    }

    (code, stmts) <- frontEnd().action(
      input,
      reader,
      output,
      colors().prompt()(prompt()).render,
      colors(),
      interp.compilerManager.complete(_, fullImports.toString, _),
      storage.fullHistory(),
      addHistory = (code) => if (code != "") {
        storage.fullHistory() = storage.fullHistory() :+ code
        history = history :+ code
      }
    )
    _ <- Signaller("INT") {
      // Put a fake `ThreadDeath` error in `lastException`, because `Thread#stop`
      // raises an error with the stack trace of *this interrupt thread*, rather
      // than the stack trace of *the mainThread*
      lastException = new ThreadDeath()
      lastException.setStackTrace(Repl.truncateStackTrace(interp.mainThread.getStackTrace))
      interp.mainThread.stop()
    }
    out <- interp.processLine(code, stmts, currentLine, false, () => currentLine += 1)
  } yield {
    printer.outStream.println()
    out
  }



  def run(): Any = {
    welcomeBanner.foreach(printer.outStream.println)
    @tailrec def loop(): Any = {
      val actionResult = action()
      Repl.handleOutput(interp, actionResult)
      Repl.handleRes(
        actionResult,
        printer.info,
        printer.error,
        lastException = _,
        colors()
      ) match{
        case None =>
          printer.outStream.println()
          loop()
        case Some(value) => value
      }
    }
    loop()
  }

  def beforeExit(exitValue: Any): Any = {
    Function.chain(interp.beforeExitHooks)(exitValue)
  }
}

object Repl{
  def handleOutput(interp: Interpreter, res: Res[Evaluated]): Unit = {
    res match{
      case Res.Skip => // do nothing
      case Res.Exit(value) => interp.compilerManager.shutdownPressy()
      case Res.Success(ev) =>
        interp.handleImports(ev.imports)
        if (interp.headFrame.frozen)
          interp.createFrame()
      case _ => ()
    }
  }
  def handleRes(res: Res[Any],
                printInfo: String => Unit,
                printError: String => Unit,
                setLastException: Throwable => Unit,
                colors: Colors): Option[Any] = {
    res match{
      case Res.Exit(value) =>
        printInfo("Bye!")
        Some(value)
      case Res.Failure(msg) =>
        printError(msg)
        None
      case Res.Exception(ex, msg) =>
        setLastException(ex)
        printError(
          Repl.showException(ex, colors.error(), fansi.Attr.Reset, colors.literal())
        )
        printError(msg)
        None
      case _ =>
        None
    }
  }
  def highlightFrame(f: StackTraceElement,
                     error: fansi.Attrs,
                     highlightError: fansi.Attrs,
                     source: fansi.Attrs) = {
    val src =
      if (f.isNativeMethod) source("Native Method")
      else if (f.getFileName == null) source("Unknown Source")
      else {
        val lineSuffix =
          if (f.getLineNumber == -1) fansi.Str("")
          else error(":") ++ source(f.getLineNumber.toString)

        source(f.getFileName) ++ lineSuffix
      }

    val prefix :+ clsName = f.getClassName.split('.').toSeq
    val prefixString = prefix.map(_+'.').mkString("")
    val clsNameString = clsName //.replace("$", error("$"))
    val method =
      error(prefixString) ++ highlightError(clsNameString) ++ error(".") ++
        highlightError(f.getMethodName)

    fansi.Str(s"  ") ++ method ++ "(" ++ src ++ ")"
  }
  val cutoff = Set("$main", "evaluatorRunPrinter")
  def truncateStackTrace(x: Array[StackTraceElement]) = {
    x.takeWhile(x => !cutoff(x.getMethodName))
  }

  def showException(ex: Throwable,
                    error: fansi.Attrs,
                    highlightError: fansi.Attrs,
                    source: fansi.Attrs) = {

    val traces = Ex.unapplySeq(ex).get.map(exception =>
      error(exception.toString + newLine +
        truncateStackTrace(exception.getStackTrace)
          .map(highlightFrame(_, error, highlightError, source))
          .mkString(newLine))
    )
    traces.mkString(newLine)
  }
}
