package ammonite

import java.io.{File, InputStream, OutputStream}

import ammonite.interp.Interpreter
import ammonite.ops._
import ammonite.runtime.{History, Storage}
import ammonite.main.Defaults
import ammonite.repl.{RemoteLogger, Repl, ReplApiImpl, SessionApiImpl}
import ammonite.util._
import ammonite.util.Util.newLine



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

    val (colors, printStream, errorPrintStream, printer) =
      Interpreter.initPrinters(outputStream, infoStream, errorStream, verboseOutput)


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
  def main(args0: Array[String]) = {
    var codeToExecute: Option[String] = None
    var verboseOutput: Boolean = true
    var ammoniteHome: Option[Path] = None
    var passThroughArgs: Seq[String] = Vector.empty
    var predefFile: Option[Path] = None
    var replApi = false
    var remoteLogging = true
    var watchScripts = false
    val replParser = new scopt.OptionParser[Main]("ammonite") {
      // Primary arguments that correspond to the arguments of
      // the `Main` configuration object
      head("ammonite", ammonite.Constants.version)

      opt[String]('p', "predef")
        .action((x, c) => c.copy(predef = x))
        .text("Any commands you want to execute at the start of the REPL session")

      opt[Unit]("no-default-predef")
        .action((x, c) => c.copy(defaultPredef = false))
        .text("Disable the default predef and run Ammonite with the minimal predef possible")

      opt[Unit]("no-remote-logging")
        .foreach(x => remoteLogging = false)
        .text("Disable remote logging of the number of times a REPL starts and runs commands")

      opt[Unit]('w', "watch")
        .foreach(x => watchScripts = true)
        .text("Watch and re-run your scripts when they change")

      opt[String]('b', "banner")
        .action((x, c) => c.copy(welcomeBanner = Some(x)))
        .text("Customize the welcome banner that gets shown when Ammonite starts")

      // Secondary arguments that correspond to different methods of
      // the `Main` configuration arguments
      opt[String]('c', "code")
        .foreach(x => codeToExecute = Some(x))
        .text("Pass in code to be run immediately in the REPL")

      arg[String]("<args>...")
        .optional()
        .unbounded()
        .foreach{ x => passThroughArgs = passThroughArgs :+ x }
        .text("Any arguments you want to pass to the Ammonite script file")
      opt[File]('h', "home")
        .valueName("<file>")
        .foreach( x => ammoniteHome = Some(Path(x, pwd)))
        .text("The home directory of the REPL; where it looks for config and caches")
      opt[String]('f', "predef-file")
        .optional()
        .foreach{ x => predefFile = Some(Path(x, pwd)) }
        .text("Lets you load your predef from a custom location")
      opt[Unit]('s', "silent")
        .foreach(x => verboseOutput = false)
        .text(
          "Make ivy logs go silent instead of printing though failures will still throw exception"
        )
      opt[Unit]("repl-api")
        .foreach(x => replApi= true)
        .text(
          """Lets you run a script with the `repl` object present; this is
            |normally not available in scripts and only provided in the
            |interactive REPL
          """.stripMargin
        )
    }

    // amm foo.sc
    // amm -h bar
    // amm foo.sc hello world
    // amm foo.sc -h bar -- hello world
    val (fileToExecute, ammoniteArgs, flatScriptArgs) = args0.lift(0) match{
      case Some(x) if x.head != '-' =>
        val fileToRun = Some(Path(args0(0), pwd))
        // running a file
        args0.indexOf("--") match {
          // all args to to file main
          case -1 => (fileToRun, Array.empty[String], args0.drop(1))
          // args before -- go to ammonite, args after -- go to file main
          case n => (fileToRun, args0.slice(1, n), args0.drop(n+1))
        }
      case _ => (None, args0, Array.empty[String]) // running the REPL, all args to to ammonite
    }

    val scriptArgs = ammonite.main.Scripts.groupArgs(flatScriptArgs)

    for(c <- replParser.parse(ammoniteArgs, Main())) {
      def main(isRepl: Boolean) = Main(
        c.predef,
        c.defaultPredef,
        new Storage.Folder(ammoniteHome.getOrElse(Defaults.ammoniteHome), isRepl) {
          override def loadPredef = {
            predefFile match{
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
        welcomeBanner = c.welcomeBanner,
        verboseOutput = verboseOutput,
        remoteLogging = remoteLogging
      )
      (fileToExecute, codeToExecute) match {
        case (None, None) => println("Loading..."); main(true).run()
        case (Some(path), None) =>
          while({
            val scriptMain = main(false)
            val (res, watched) = scriptMain.runScript(path, scriptArgs, replApi)
            res match {
              case Res.Failure(exOpt, msg) =>
                Console.err.println(msg)
                if (!watchScripts) System.exit(1)
              case Res.Exception(ex, s) =>
                val trace = ex.getStackTrace
                val i = trace.indexWhere(_.getMethodName == "$main") + 1
                ex.setStackTrace(trace.take(i))
                if (!watchScripts) throw ex
                else ex.printStackTrace()
              case Res.Success(value) =>
                if (value != ()){
                  pprint.PPrinter.BlackWhite.pprintln(value)
                }
              case Res.Skip   => // do nothing on success, everything's already happened
            }
            if (!watchScripts) false
            else{
              println(s"Watching for changes to ${watched.length} files... (Ctrl-C to exit)")
              while(
                watched.forall{
                  case (file, lastMTime) => Interpreter.mtimeIfExists(file) == lastMTime
                }
              ){
                Thread.sleep(100)
              }


              true
            }
          })()


        case (None, Some(code)) => main(false).runCode(code, replApi)

      }

    }

    // Exit explicitly to kill any daemon threads that are running
    System.exit(0)
  }

  def maybeDefaultPredef(enabled: Boolean, predef: String) =
    if (enabled) predef else ""


}
