package ammonite

import java.io.{InputStream, OutputStream, PrintStream}

import ammonite.interp.Interpreter
import ammonite.ops._
import ammonite.runtime.{Frame, Storage}
import ammonite.main._
import ammonite.repl.{RemoteLogger, Repl}
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
  * @param predef Any additional code you want to run before the REPL session
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
case class Main(predef: String = "",
                defaultPredef: Boolean = true,
                storageBackend: Storage = new Storage.Folder(Defaults.ammoniteHome),
                wd: Path = ammonite.ops.pwd,
                welcomeBanner: Option[String] = Some(Defaults.welcomeBanner),
                inputStream: InputStream = System.in,
                outputStream: OutputStream = System.out,
                errorStream: OutputStream = System.err,
                verboseOutput: Boolean = true,
                remoteLogging: Boolean = true,
                colors: Colors = Colors.Default){

  /**
    * Instantiates an ammonite.Repl using the configuration
    */
  def instantiateRepl(replArgs: IndexedSeq[Bind[_]] = Vector.empty,
                     remoteLogger: Option[RemoteLogger]) = {
    val augmentedPredef = Main.maybeDefaultPredef(
      defaultPredef,
      Defaults.replPredef + Defaults.predefString
    )

    new Repl(
      inputStream, outputStream, errorStream,
      storage = storageBackend,
      defaultPredef = augmentedPredef,
      mainPredef = predef,
      wd = wd,
      welcomeBanner = welcomeBanner,
      replArgs = replArgs,
      remoteLogger = remoteLogger,
      initialColors = colors
    )
  }

  def instantiateInterpreter() = {
    val augmentedPredef = Main.maybeDefaultPredef(defaultPredef, Defaults.predefString)

    val (colorsRef, _, _, printer) = Interpreter.initPrinters(
      colors,
      outputStream,
      errorStream,
      verboseOutput
    )
    val frame = Frame.createInitial()

    val interp: Interpreter = new Interpreter(
      printer,
      storageBackend,
      Seq(
        PredefInfo(Name("defaultPredef"), augmentedPredef, false, None),
        PredefInfo(Name("predef"), predef, false, None)
      ),
      Vector.empty,
      wd,
      colorsRef,
      verboseOutput,
      () => frame
    )
    interp.initializePredef() match{
      case None => Right(interp)
      case Some(problems) => Left(problems)
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
  def run(replArgs: Bind[_]*): (Res[Any], Seq[(Path, Long)]) = {

    val remoteLogger =
      if (!remoteLogging) None
      else Some(new ammonite.repl.RemoteLogger(storageBackend.getSessionId))

    remoteLogger.foreach(_.apply("Boot"))

    val repl = instantiateRepl(replArgs.toIndexedSeq, remoteLogger)

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

        try{
          val exitValue = Res.Success(repl.run())
          (exitValue.map(repl.beforeExit), repl.interp.watchedFiles)
        }finally{
          remoteLogger.foreach(_.close())
        }
    }
  }

  /**
    * Run a Scala script file! takes the path to the file as well as an array
    * of `args` and a map of keyword `kwargs` to pass to that file.
    */
  def runScript(path: Path,
                scriptArgs: Seq[(String, Option[String])])
                : (Res[Any], Seq[(Path, Long)]) = {

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
              val success = runner.runScript(Path(head, pwd), rest)
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
                                  run: Main => (Res[T], Seq[(Path, Long)])): Boolean = {
    val (result, watched) = initMain(isRepl) match{
      case Left((msg, watched)) => (Res.Failure(msg), watched)
      case Right(main) => run(main)
    }
    val success = handleWatchRes(result)
    if (!cliConfig.watch) success
    else{
      watchAndWait(watched)
      watchLoop(isRepl, run)
    }
  }

  def runScript(scriptPath: Path, scriptArgs: List[String]) =
    watchLoop(false, _.runScript(scriptPath, Scripts.groupArgs(scriptArgs)))

  def runCode(code: String) = watchLoop(false, _.runCode(code))

  def runRepl(): Unit = watchLoop(true, _.run())

  def watchAndWait(watched: Seq[(Path, Long)]) = {
    printInfo(s"Watching for changes to ${watched.length} files... (Ctrl-C to exit)")
    def statAll() = watched.forall{ case (file, lastMTime) =>
      Interpreter.pathSignature(file) == lastMTime
    }

    while(statAll()) Thread.sleep(100)
  }

  def handleWatchRes[T](res: Res[T]) = {
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
        if (value != ()) outprintStream.println(pprint.PPrinter.BlackWhite(value))
        true

      case Res.Skip   => true // do nothing on success, everything's already happened
    }
    success
  }


  def initMain(isRepl: Boolean) = {
    val loadedPredef = cliConfig.predefFile match{
      case None => Right(None)
      case Some(file) =>
        try Right(Some((read(file), file)))
        catch {case e: java.nio.file.NoSuchFileException =>
          // If we cannot find the user's explicitly-specified predef, fail
          // loudly. We can happily fail-silently the "default" predef files,
          // since the user may or may not have created one, but since the user
          // explicitly specified one here we assume that it has to exist.
          Left(("Script file not found: " + file, Seq(file -> 0L)))
        }
    }
    val storage = loadedPredef.right.map{
      case None => new Storage.Folder(cliConfig.home, isRepl)
      case Some((predefCode, predefFile)) =>
        new Storage.Folder(cliConfig.home, isRepl) {
          override def loadPredef = Some((predefCode, predefFile))
        }
    }
    storage.right.map{ storage =>
      Main(
        cliConfig.predef,
        cliConfig.defaultPredef,
        storage,
        inputStream = stdIn,
        outputStream = stdOut,
        errorStream = stdErr,
        welcomeBanner = cliConfig.welcomeBanner,
        verboseOutput = cliConfig.verboseOutput,
        remoteLogging = cliConfig.remoteLogging,
        colors = colors
      )
    }

  }
}