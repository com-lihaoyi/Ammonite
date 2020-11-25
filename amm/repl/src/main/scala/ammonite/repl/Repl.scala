package ammonite.repl

import java.io.{InputStream, InputStreamReader, OutputStream}
import java.nio.file.Path

import ammonite.repl.api.{FrontEnd, History, ReplAPI, ReplLoad}
import ammonite.runtime._
import ammonite.terminal.Filter
import ammonite.util.InterfaceExtensions._
import ammonite.util.Util.{newLine, normalizeNewlines}
import ammonite.util._
import ammonite.compiler.Parsers
import ammonite.compiler.iface.{CodeWrapper, CompilerLifecycleManager, Imports, Parser}
import ammonite.interp.Interpreter
import coursierapi.Dependency

import scala.annotation.tailrec

class Repl(compilerManager: CompilerLifecycleManager,
           parser: Parser,
           input: InputStream,
           output: OutputStream,
           error: OutputStream,
           storage: Storage,
           baseImports: Imports,
           basePredefs: Seq[PredefInfo],
           customPredefs: Seq[PredefInfo],
           wd: os.Path,
           welcomeBanner: Option[String],
           replArgs: IndexedSeq[Bind[_]] = Vector.empty,
           initialColors: Colors = Colors.Default,
           replCodeWrapper: CodeWrapper,
           scriptCodeWrapper: CodeWrapper,
           alreadyLoadedDependencies: Seq[Dependency],
           importHooks: Map[Seq[String], ImportHook],
           initialClassLoader: ClassLoader =
             classOf[ammonite.repl.api.ReplAPI].getClassLoader,
           classPathWhitelist: Set[Seq[String]]) { repl =>

  var prompt = () => "@ "

  val frontEnd = Ref[FrontEnd](
    if (scala.util.Properties.isWin)
      ammonite.repl.FrontEnds.JLineWindows
    else
      AmmoniteFrontEnd(Filter.empty)
  )

  var lastException: Throwable = null

  var history = new History(Array())

  val (colors, printer) =
    Interpreter.initPrinters(initialColors, output, error, true)

  val argString = replArgs.zipWithIndex.map{ case (b, idx) =>
    s"""
    val ${b.name} =
      ammonite.repl.ReplBridge.value.replArgs($idx).value.asInstanceOf[${b.typeTag.tpe}]
    """
  }.mkString(newLine)

  val frames = Ref(List(
    Frame.createInitial(initialClassLoader, forking = classPathWhitelist.isEmpty)
  ))

  /**
    * The current line number of the REPL, used to make sure every snippet
    * evaluated can have a distinct name that doesn't collide.
    */
  var currentLine = 0


  val sess0 = new SessionApiImpl(frames)

  def imports = frames().head.imports
  def fullImports = interp.predefImports ++ imports

  def usedEarlierDefinitions = frames().head.usedEarlierDefinitions

  val interp = new Interpreter(
    compilerManager,
    parser,
    printer,
    storage,
    wd,
    verboseOutput = true,
    getFrame = () => frames().head,
    createFrame = () => { val f = sess0.childFrame(frames().head); frames() = f :: frames(); f },
    initialClassLoader = initialClassLoader,
    replCodeWrapper = replCodeWrapper,
    scriptCodeWrapper = scriptCodeWrapper,
    alreadyLoadedDependencies = alreadyLoadedDependencies,
    importHooks,
    classPathWhitelist = classPathWhitelist
  )

  val bridges = Seq(
    (
      "ammonite.repl.ReplBridge",
      "repl",
      new ReplAPI {
        def replArgs = repl.replArgs
        def printer = repl.printer
        val colors = repl.colors
        def sess = repl.sess0
        def prompt = repl.prompt()
        def prompt_=(prompt: => String): Unit =
          repl.prompt = () => prompt
        val frontEnd = repl.frontEnd

        val pprinter: Ref[pprint.PPrinter] = Ref.live(() =>
          pprint.PPrinter.Color.copy(
            defaultHeight = height / 2,
            defaultWidth = width,
            colorLiteral = colors().literal(),
            colorApplyPrefix = colors().prefix(),
            additionalHandlers = PPrints.replPPrintHandlers
          )
        )

        def lastException = repl.lastException
        def fullRawHistory = storage.fullHistory().array
        def rawHistory = repl.history.array
        def newCompiler() = interp.compilerManager.forceInit()
        def compiler = null
        def interactiveCompiler = null
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

          def exec(file: Path): Unit = {
            interp.watch(os.Path(file))
            apply(normalizeNewlines(os.read(os.Path(file))))
          }
        }
      }
    )
  )

  def initializePredef() = interp.initializePredef(basePredefs, customPredefs, bridges, baseImports)

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

    _ <- Signaller("INT") {
      // Put a fake `ThreadDeath` error in `lastException`, because `Thread#stop`
      // raises an error with the stack trace of *this interrupt thread*, rather
      // than the stack trace of *the mainThread*
      lastException = new ThreadDeath()
      lastException.setStackTrace(Repl.truncateStackTrace(interp.mainThread.getStackTrace))
      interp.mainThread.stop()
    }
    (code, stmts) <- frontEnd().action(
      input,
      reader,
      output,
      colors().prompt()(prompt()).render,
      colors(),
      interp.compilerManager.complete(_, fullImports.repr, _),
      storage.fullHistory(),
      addHistory = (code) => if (code != "") {
        storage.fullHistory() = storage.fullHistory() :+ code
        history = history :+ code
      }
    )
    out <- interp.processLine(code, stmts, currentLine, false, () => currentLine += 1)
  } yield {
    printer.outStream.println()
    out
  }



  def run(): Object = {
    welcomeBanner.foreach(printer.outStream.println)
    @tailrec def loop(): Object = {
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

  def beforeExit(exitValue: Object): Object = {
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
                colors: Colors): Option[Object] = {
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

  def getClassPathWhitelist(thin: Boolean): Set[Seq[String]] = {
    if (!thin) Set.empty
    else {
      os.read
        .lines(os.resource / "ammonite-api-whitelist.txt")
        .map(_.split('/').toSeq)
        .toSet
    }
  }
}
