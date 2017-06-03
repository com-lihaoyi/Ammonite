package ammonite

import java.io.{InputStream, OutputStream}

import ammonite.interp.Interpreter
import ammonite.ops._
import ammonite.runtime.{History, Storage}
import ammonite.main._
import ammonite.repl.{RemoteLogger, Repl, ReplApiImpl, SessionApiImpl}
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
  * @param infoStream Miscellaneous logging output when running Ammonite. This
  *                   is typically stuff you want to see when running interactively,
  *                   but not something you want to see when e.g. you forward a
  *                   script's output to a file. This by default it goes to System.err
  * @param errorStream Error output when things go bad, typically System.err
  */
case class Main(predef: String = "",
                defaultPredef: Boolean = true,
                storageBackend: Storage = new Storage.Folder(Defaults.ammoniteHome),
                wd: Path = ammonite.ops.pwd,
                welcomeBanner: Option[String] = Some(Defaults.welcomeBanner),
                inputStream: InputStream = System.in,
                outputStream: OutputStream = System.out,
                infoStream: OutputStream = System.err,
                errorStream: OutputStream = System.err,
                verboseOutput: Boolean = true,
                remoteLogging: Boolean = true){

  /**
    * Instantiates an ammonite.Repl using the configuration
    */
  def instantiateRepl(replArgs: Seq[Bind[_]] = Nil,
                     remoteLogger: Option[RemoteLogger]) = {
    val augmentedPredef = Main.maybeDefaultPredef(
      defaultPredef,
      Defaults.replPredef + Defaults.predefString
    )

    new Repl(
      inputStream, outputStream, infoStream, errorStream,
      storage = storageBackend,
      defaultPredef = augmentedPredef,
      mainPredef = predef,
      wd = wd,
      welcomeBanner = welcomeBanner,
      replArgs = replArgs,
      remoteLogger = remoteLogger
    )
  }

  def instantiateInterpreter(replApi: Boolean) = {
    val augmentedPredef = Main.maybeDefaultPredef(defaultPredef, Defaults.predefString)

    val (colors, printStream, errorPrintStream, printer) = Interpreter.initPrinters(
      Colors.BlackWhite, outputStream, infoStream, errorStream, verboseOutput
    )


    val interp: Interpreter = new Interpreter(
      printer,
      storageBackend,
      Seq(
        PredefInfo(Name("defaultPredef"), augmentedPredef, false, None),
        PredefInfo(Name("predef"), predef, false, None)
      ),
      i =>
        if (!replApi) Nil
        else {
          val replApi = new ReplApiImpl(
            i,
            80,
            80,
            colors,
            Ref(null),
            Ref(null),
            new History(Vector.empty),
            new SessionApiImpl(i.compilerManager.frames),
            Vector()
          )
          Seq(("ammonite.repl.ReplBridge", "repl", replApi, () => ()))
        },
      wd,
      verboseOutput
    )
    interp
  }

  def run(replArgs: Bind[_]*) = {

    val remoteLogger =
      if (!remoteLogging) None
      else Some(new ammonite.repl.RemoteLogger(storageBackend.getSessionId))

    remoteLogger.foreach(_.apply("Boot"))

    val repl = instantiateRepl(replArgs, remoteLogger)

    try{
      val exitValue = repl.run()
      repl.beforeExit(exitValue)
    }finally{
      remoteLogger.foreach(_.close())
    }


  }

  /**
    * Run a Scala script file! takes the path to the file as well as an array
    * of `args` and a map of keyword `kwargs` to pass to that file.
    */
  def runScript(path: Path,
                scriptArgs: Seq[(String, Option[String])],
                replApi: Boolean = false): (Res[Any], Seq[(Path, Option[Long])]) = {

    val interp = instantiateInterpreter(replApi)
    (main.Scripts.runScript(wd, path, interp, scriptArgs), interp.watchedFiles)
  }

  /**
    * Run a snippet of code
    */
  def runCode(code: String, replApi: Boolean = false) = {
    val interp = instantiateInterpreter(replApi)
    interp.interpApi.load(code)
  }
}

object Main{

  /**
    * The command-line entry point, which does all the argument parsing before
    * delegating to [[Main.run]]
    */
  def main(args0: Array[String]): Unit = {

    val (cliConfig, leftoverArgs) =
      Cli.groupArgs(args0.toList, Cli.ammoniteArgSignature, Cli.Config()) match{
        case Right(v) => v
        case Left(errorMsg) =>
          println(errorMsg)
          sys.exit(1)
      }

    if (cliConfig.help) {
      println(Cli.ammoniteHelp)
      sys.exit(0)
    }

    (cliConfig.code, leftoverArgs) match{
      case (Some(code), Nil) =>
        fromConfig(cliConfig, true).runCode(code, cliConfig.replApi)

      case (None, Nil) =>
        println("Loading...")
        fromConfig(cliConfig, true).run()

      case (None, head :: rest) if head.startsWith("-") =>
        println("Unknown Ammonite argument: " + head)
        println(Cli.ammoniteHelp)
        sys.exit(1)

      case (None, head :: rest) =>
        val success = runScript(Path(head, pwd), rest, cliConfig)
        if (success) sys.exit(0)
        else sys.exit(1)
    }
  }

  @tailrec def runScript(scriptPath: Path,
                         scriptArgs: List[String],
                         cliConfig: Cli.Config): Boolean = {
    val (success, watched) = runScriptAndPrint(
      scriptPath,
      scriptArgs,
      cliConfig,
      fromConfig(cliConfig, false)
    )
    if (!cliConfig.watch) success
    else{
      println(s"Watching for changes to ${watched.length} files... (Ctrl-C to exit)")
      def statAll() = watched.forall{ case (file, lastMTime) =>
        Interpreter.mtimeIfExists(file) == lastMTime
      }

      while(statAll()) Thread.sleep(100)

      runScript(scriptPath, scriptArgs, cliConfig)
    }
  }

  def runScriptAndPrint(scriptPath: Path,
                        flatArgs: List[String],
                        c: Cli.Config,
                        scriptMain: Main): (Boolean, Seq[(Path, Option[Long])]) = {

    val (res, watched) = scriptMain.runScript(
      scriptPath,
      Scripts.groupArgs(flatArgs),
      c.replApi
    )
    val success = res match {
      case Res.Failure(exOpt, msg) =>
        println("FAILURE")
        Console.err.println(msg)
        false
      case Res.Exception(ex, s) =>
        val trace = ex.getStackTrace
        val i = trace.indexWhere(_.getMethodName == "$main") + 1
        ex.setStackTrace(trace.take(i))
        ex.printStackTrace()
        false

      case Res.Success(value) =>
        if (value != ()) pprint.PPrinter.BlackWhite.pprintln(value)
        true

      case Res.Skip   => true // do nothing on success, everything's already happened
    }
    (success, watched)
  }

  def fromConfig(cliConfig: Cli.Config, isRepl: Boolean) = Main(
    cliConfig.predef,
    cliConfig.defaultPredef,
    new Storage.Folder(cliConfig.home, isRepl) {
      override def loadPredef = {
        cliConfig.predefFile match{
          case None => super.loadPredef
          case Some(file) =>
            try {
              (read(file), Some(file))
            } catch {
              case e: java.nio.file.NoSuchFileException => ("", None)
            }
        }
      }
    },
    welcomeBanner = cliConfig.welcomeBanner,
    verboseOutput = cliConfig.verboseOutput,
    remoteLogging = cliConfig.remoteLogging
  )

  
  def maybeDefaultPredef(enabled: Boolean, predef: String) =
    if (enabled) predef else ""
}
