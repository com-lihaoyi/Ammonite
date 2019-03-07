package ammonite

import java.io.{InputStream, OutputStream, PrintStream}
import java.nio.file.NoSuchFileException

import ammonite.interp.{Interpreter, Preprocessor}

import ammonite.runtime.{Frame, Storage}
import ammonite.main._
import ammonite.repl.Repl
import ammonite.util.Util.newLine
import ammonite.util._

import scala.annotation.tailrec



/**
  * Contains the various entry points to the Ammonite REPL.
  *
  * Configuration of the basic REPL is done by passing in arguments when
  * constructing the [[Main]] instance, and the various entrypoints such
  * as [[run]] [[runScript]] and so on are methods on that instance.
  *
  * It is more or less equivalent to the [[ammonite.repl.Repl]] object itself, and has
  * a similar set of parameters, but does not have any of the [[ammonite.repl.Repl]]'s
  * implementation-related code and provides a more convenient set of
  * entry-points that a user can call.
  *
  * Note that the [[instantiateRepl]] function generates a new [[Repl]]
  * every time it is called!
  *
  * @param predefCode Any additional code you want to run before the REPL session
  *               starts. Can contain multiple blocks separated by `@`s
  * @param defaultPredef Do you want to include the "standard" predef imports
  *                      provided by Ammonite? These include tools like `time`,
  *                      `grep`, the `|` or `|?` pipes from ammonite-ops, and
  *                      other helpers. Can be disabled to give a clean
  *                      namespace for you to fill using your own predef.
  * @param storageBackend Where will all of Ammonite's persistent data get
  *                       stored? Things like any `predef.sc` file,
  *                       compilation/ivy caches, etc.. Defaults include
  *                       [[Storage.Folder]] and [[Storage.InMemory]], though
  *                       you can create your own.
  * @param wd The working directory of the REPL; when it load scripts, where
  *           the scripts will be considered relative to when assigning them
  *           packages
  *
  * @param inputStream Where input to the Repl is coming from, typically System.in,
  *                    but it could come from somewhere else e.g. across the
  *                    network in the case of the SshdRepl
  * @param outputStream Primary output of code run using Ammonite
  * @param errorStream Error output when things go bad, typically System.err; also
  *                    gets sent miscellaneous info messages that aren't strictly
  *                    part of the REPL or script's output
  */
case class Main(predefCode: String = "",
                predefFile: Option[os.Path] = None,
                defaultPredef: Boolean = true,
                storageBackend: Storage = new Storage.Folder(Defaults.ammoniteHome),
                wd: os.Path = os.pwd,
                welcomeBanner: Option[String] = Some(Defaults.welcomeBanner),
                inputStream: InputStream = System.in,
                outputStream: OutputStream = System.out,
                errorStream: OutputStream = System.err,
                verboseOutput: Boolean = true,
                remoteLogging: Boolean = true,
                colors: Colors = Colors.Default,
                replCodeWrapper: Preprocessor.CodeWrapper = Preprocessor.CodeWrapper,
                scriptCodeWrapper: Preprocessor.CodeWrapper = Preprocessor.CodeWrapper,
                alreadyLoadedDependencies: Seq[coursier.Dependency] =
                  Defaults.alreadyLoadedDependencies()){

  def loadedPredefFile = predefFile match{
    case Some(path) =>
      try Right(Some(PredefInfo(Name("FilePredef"), os.read(path), false, Some(path))))
      catch{case e: NoSuchFileException =>
        Left((Res.Failure("Unable to load predef file " + path), Seq(path -> 0L)))
      }
    case None => Right(None)
  }
  /**
    * Instantiates an ammonite.Repl using the configuration
    */
  def instantiateRepl(replArgs: IndexedSeq[Bind[_]] = Vector.empty) = {


    loadedPredefFile.right.map{ predefFileInfoOpt =>
      val augmentedPredef = Main.maybeDefaultPredef(
        defaultPredef,
        Defaults.replPredef + Defaults.predefString + Main.extraPredefString
      )

      val argString = replArgs.zipWithIndex.map{ case (b, idx) =>
        s"""
        val ${b.name} = ammonite
          .repl
          .ReplBridge
          .value
          .Internal
          .replArgs($idx)
          .value
          .asInstanceOf[${b.typeTag.tpe}]
        """
      }.mkString(newLine)

      new Repl(
        inputStream, outputStream, errorStream,
        storage = storageBackend,
        basePredefs = Seq(
          PredefInfo(Name("DefaultPredef"), augmentedPredef, true, None),
          PredefInfo(Name("ArgsPredef"), argString, false, None)
        ),
        customPredefs = predefFileInfoOpt.toSeq ++ Seq(
          PredefInfo(Name("CodePredef"), predefCode, false, Some(wd/"(console)"))
        ),
        wd = wd,
        welcomeBanner = welcomeBanner,
        replArgs = replArgs,
        initialColors = colors,
        replCodeWrapper = replCodeWrapper,
        scriptCodeWrapper = scriptCodeWrapper,
        alreadyLoadedDependencies = alreadyLoadedDependencies
      )
    }

  }

  def instantiateInterpreter() = {
    loadedPredefFile.right.flatMap { predefFileInfoOpt =>
      val augmentedPredef = Main.maybeDefaultPredef(
        defaultPredef,
        Defaults.predefString + Main.extraPredefString
      )

      val (colorsRef, printer) = Interpreter.initPrinters(
        colors,
        outputStream,
        errorStream,
        verboseOutput
      )
      val frame = Frame.createInitial()

      val interp: Interpreter = new Interpreter(
        printer,
        storageBackend,
        basePredefs = Seq(
          PredefInfo(Name("DefaultPredef"), augmentedPredef, false, None)
        ),
        predefFileInfoOpt.toSeq ++ Seq(
          PredefInfo(Name("CodePredef"), predefCode, false, None)
        ),
        Vector.empty,
        wd,
        colorsRef,
        verboseOutput,
        () => frame,
        () => throw new Exception("session loading / saving not possible here"),
        replCodeWrapper,
        scriptCodeWrapper,
        alreadyLoadedDependencies = alreadyLoadedDependencies
      )
      interp.initializePredef() match{
        case None => Right(interp)
        case Some(problems) => Left(problems)
      }
    }

  }

  /**
    * Run the REPL, with any additional bindings you wish to provide.
    *
    * Returns an `Any` representing any value that the user passed into the
    * `exit` call when closing the REPL (defaults to `(): Unit`). Also returns
    * a sequence of paths that were watched as a result of this REPL run, in
    * case you wish to re-start the REPL when any of them change.
    */
  def run(replArgs: Bind[_]*): (Res[Any], Seq[(os.Path, Long)]) = {


    instantiateRepl(replArgs.toIndexedSeq) match{
      case Left(missingPredefInfo) => missingPredefInfo
      case Right(repl) =>
        repl.initializePredef().getOrElse{
          // Warm up the compilation logic in the background, hopefully while the
          // user is typing their first command, so by the time the command is
          // submitted it can be processed by a warm compiler
          val warmupThread = new Thread(new Runnable{
            def run() = repl.warmup()
          })
          // This thread will terminal eventually on its own, but if the
          // JVM wants to exit earlier this thread shouldn't stop it
          warmupThread.setDaemon(true)
          warmupThread.start()

          val exitValue = Res.Success(repl.run())
          (exitValue.map(repl.beforeExit), repl.interp.watchedFiles)
        }
    }
  }

  /**
    * Run a Scala script file! takes the path to the file as well as an array
    * of `args` and a map of keyword `kwargs` to pass to that file.
    */
  def runScript(path: os.Path,
                scriptArgs: Seq[(String, Option[String])])
                : (Res[Any], Seq[(os.Path, Long)]) = {

    instantiateInterpreter() match{
      case Right(interp) =>
        val result = main.Scripts.runScript(wd, path, interp, scriptArgs)
        (result, interp.watchedFiles)
      case Left(problems) => problems
    }
  }

  /**
    * Run a snippet of code
    */
  def runCode(code: String) = {
    instantiateInterpreter() match{
      case Right(interp) =>
        val res = interp.processExec(code, 0, () => ())
        (res, interp.watchedFiles)
      case Left(problems) => problems
    }
  }
}

object Main{

  /**
    * The command-line entry point, which does all the argument parsing before
    * delegating to [[Main.run]]
    */
  def main(args0: Array[String]): Unit = {
    // set proxy properties from env
    // Not in `main0`, since `main0` should be able to be run as part of the
    // test suite without mangling the global properties of the JVM process
    ProxyFromEnv.setPropProxyFromEnv()

    val success = main0(args0.toList, System.in, System.out, System.err)
    if (success) sys.exit(0)
    else sys.exit(1)
  }

  /**
    * The logic of [[main]], in a form that doesn't call `sys.exit` and thus
    * can be unit tested without spinning up lots of separate, expensive
    * processes
    */
  def main0(args: List[String],
            stdIn: InputStream,
            stdOut: OutputStream,
            stdErr: OutputStream): Boolean = {
    val printErr = new PrintStream(stdErr)
    val printOut = new PrintStream(stdOut)
    // We have to use explicit flatmaps instead of a for-comprehension here
    // because for-comprehensions fail to compile complaining about needing
    // withFilter
    Cli.groupArgs(args, Cli.ammoniteArgSignature, Cli.Config()) match{
      case Left(msg) =>
        printErr.println(msg)
        false
      case Right((cliConfig, leftoverArgs)) =>
        if (cliConfig.help) {
          printOut.println(Cli.ammoniteHelp)
          true
        }else{

          val runner = new MainRunner(cliConfig, printOut, printErr, stdIn, stdOut, stdErr)
          (cliConfig.code, leftoverArgs) match{
            case (Some(code), Nil) =>
              runner.runCode(code)

            case (None, Nil) =>
              runner.printInfo("Loading...")
              runner.runRepl()
              true

            case (None, head :: rest) if head.startsWith("-") =>

              val failureMsg =
                "Unknown Ammonite option: " + head + Util.newLine +
                "Use --help to list possible options"

              runner.printError(failureMsg)
              false

            case (None, head :: rest) =>
              val success = runner.runScript(os.Path(head, os.pwd), rest)
              success
          }
        }
    }
  }

  def maybeDefaultPredef(enabled: Boolean, predef: String) =
    if (enabled) predef else ""


  /**
    * Detects if the console is interactive; lets us make console-friendly output
    * (e.g. ansi color codes) if it is, and script-friendly output (no ansi codes)
    * if it's not
    *
    * https://stackoverflow.com/a/1403817/871202
    */
  def isInteractive() = System.console() != null

  val extraPredefString = s"""
    |import ammonite.main.Router.{doc, main}
    |import ammonite.main.Scripts.pathScoptRead
    |""".stripMargin

}

/**
  * Bundles together:
  *
  * - All the code relying on [[cliConfig]]
  * - Handling for the common input/output streams and print-streams
  * - Logic around the watch-and-rerun flag
  */
class MainRunner(cliConfig: Cli.Config,
                 outprintStream: PrintStream,
                 errPrintStream: PrintStream,
                 stdIn: InputStream,
                 stdOut: OutputStream,
                 stdErr: OutputStream){

  val colors =
    if(cliConfig.colored.getOrElse(Main.isInteractive())) Colors.Default
    else Colors.BlackWhite

  def printInfo(s: String) = errPrintStream.println(colors.info()(s))
  def printError(s: String) = errPrintStream.println(colors.error()(s))

  @tailrec final def watchLoop[T](isRepl: Boolean,
                                  printing: Boolean,
                                  run: Main => (Res[T], Seq[(os.Path, Long)])): Boolean = {
    val (result, watched) = run(initMain(isRepl))

    val success = handleWatchRes(result, printing)
    if (!cliConfig.watch) success
    else{
      watchAndWait(watched)
      watchLoop(isRepl, printing, run)
    }
  }

  def runScript(scriptPath: os.Path, scriptArgs: List[String]) =
    watchLoop(
      isRepl = false,
      printing = true,
      _.runScript(scriptPath, Scripts.groupArgs(scriptArgs))
    )

  def runCode(code: String) = watchLoop(isRepl = false, printing = false, _.runCode(code))

  def runRepl(): Unit = watchLoop(isRepl = true, printing = false, _.run())

  def watchAndWait(watched: Seq[(os.Path, Long)]) = {
    printInfo(s"Watching for changes to ${watched.length} files... (Ctrl-C to exit)")
    def statAll() = watched.forall{ case (file, lastMTime) =>
      Interpreter.pathSignature(file) == lastMTime
    }

    while(statAll()) Thread.sleep(100)
  }

  def handleWatchRes[T](res: Res[T], printing: Boolean) = {
    val success = res match {
      case Res.Failure(msg) =>
        printError(msg)
        false
      case Res.Exception(ex, s) =>
        errPrintStream.println(
          Repl.showException(ex, colors.error(), fansi.Attr.Reset, colors.literal())
        )
        false

      case Res.Success(value) =>
        if (printing && value != ()) outprintStream.println(pprint.PPrinter.BlackWhite(value))
        true

      case Res.Skip   => true // do nothing on success, everything's already happened
    }
    success
  }


  def initMain(isRepl: Boolean) = {
    val storage = if (!cliConfig.homePredef) {
      new Storage.Folder(cliConfig.home, isRepl) {
        override def loadPredef = None
      }
    }else{
      new Storage.Folder(cliConfig.home, isRepl)
    }

    val codeWrapper =
      if (cliConfig.classBased)
        Preprocessor.CodeClassWrapper
      else
        Preprocessor.CodeWrapper

    Main(
      cliConfig.predefCode,
      cliConfig.predefFile,
      cliConfig.defaultPredef,
      storage,
      wd = cliConfig.wd,
      inputStream = stdIn,
      outputStream = stdOut,
      errorStream = stdErr,
      welcomeBanner = cliConfig.welcomeBanner,
      verboseOutput = cliConfig.verboseOutput,
      remoteLogging = cliConfig.remoteLogging,
      colors = colors,
      replCodeWrapper = codeWrapper,
      scriptCodeWrapper = codeWrapper
    )

  }
}