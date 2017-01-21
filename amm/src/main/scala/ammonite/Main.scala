package ammonite

import java.io.{File, InputStream, OutputStream}

import ammonite.interp.Interpreter
import ammonite.ops._
import ammonite.runtime.{History, Storage}
import ammonite.main.Defaults
import ammonite.repl.{Repl, ReplApiImpl, SessionApiImpl}
import ammonite.util._
import ammonite.util.Util.newLine



/**
  * Contains the various entry points to the Ammonite REPL.
  *
  * Configuration of the basic REPL is done by passing in arguments when
  * constructing the [[Main]] instance, and the various entrypoints such
  * as [[run]] [[runScript]] and so on are methods on that instance.
  *
  * It is more or less equivalent to the [[ammonite.main.Repl]] object itself, and has
  * a similar set of parameters, but does not have any of the [[ammonite.main.Repl]]'s
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
  */
case class Main(predef: String = "",
                defaultPredef: Boolean = true,
                storageBackend: Storage = new Storage.Folder(Defaults.ammoniteHome),
                wd: Path = ammonite.ops.pwd,
                welcomeBanner: Option[String] = Some(Defaults.welcomeBanner),
                inputStream: InputStream = System.in,
                outputStream: OutputStream = System.out,
                errorStream: OutputStream = System.err,
                verboseOutput: Boolean = true
               ){
  /**
    * Instantiates an ammonite.Repl using the configuration
    */
  def instantiateRepl(replArgs: Seq[Bind[_]] = Nil, interpArgs: Seq[String] = Nil) = {
    val augmentedPredef = Main.maybeDefaultPredef(defaultPredef, Defaults.predefString)

    new Repl(
      inputStream, outputStream, errorStream,
      storage = storageBackend,
      predef = augmentedPredef + newLine + predef,
      wd = wd,
      welcomeBanner = welcomeBanner,
      replArgs = replArgs,
      interpArgs = interpArgs
    )
  }

  def instantiateInterpreter(replApi: Boolean, args: Seq[String] = Nil) = {
    val augmentedPredef = Main.maybeDefaultPredef(defaultPredef, Defaults.predefString)

    val (colors, printStream, errorPrintStream, printer) =
      Interpreter.initPrinters(outputStream, errorStream, verboseOutput)


    val interp: Interpreter = new Interpreter(
      printer,
      storageBackend,
      Seq(
        Name("defaultPredef") -> augmentedPredef,
        Name("predef") -> predef
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
            new SessionApiImpl(i.eval),
            Vector()
          )
          Seq(("ammonite.repl.ReplBridge", "repl", replApi))
        },
      wd,
      args,
      verboseOutput
    )
    interp
  }
  def run(interpArgs: Seq[String], replArgs: Bind[_]*) = {
    val repl = instantiateRepl(replArgs, interpArgs)
    repl.run()
  }

  /**
    * Run a Scala script file! takes the path to the file as well as an array
    * of `args` and a map of keyword `kwargs` to pass to that file.
    */
  def runScript(path: Path,
                args: Seq[String],
                kwargs: Seq[(String, String)],
                interpArgs: Seq[String],
                replApi: Boolean = false): Res[Imports] = {

    val interp = instantiateInterpreter(replApi, interpArgs)
    main.Scripts.runScript(wd, path, interp, args, kwargs)
  }

  /**
    * Run a snippet of code
    */
  def runCode(code: String, interpArgs: Seq[String], replApi: Boolean = false) = {
    val interp = instantiateInterpreter(replApi, interpArgs)
    interp.interpApi.load(code)
  }
}

object Main{

  /**
    * The command-line entry point, which does all the argument parsing before
    * delegating to [[Main.run]]
    */
  def main(args0: Array[String]) = {
    var fileToExecute: Option[Path] = None
    var codeToExecute: Option[String] = None
    var verboseOutput: Boolean = true
    var ammoniteHome: Option[Path] = None
    var passThroughArgs: Seq[String] = Vector.empty
    var predefFiles: Seq[Path] = Vector.empty
    var continually = false
    var replApi = false
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

      // Secondary arguments that correspond to different methods of
      // the `Main` configuration arguments
      arg[String]("<file-args>...")
        .optional()
        .foreach{ x => fileToExecute = Some(Path(x, pwd)) }
        .text("The Ammonite script file you want to execute")
      opt[String]('c', "code")
        .foreach(x => codeToExecute = Some(x))
        .text("Pass in code to be run immediately in the REPL")

      opt[Unit]('x', "execute")
        .text(
          "Shim for backwards compatibility - will be removed"
        )
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
        .unbounded()
        .foreach{ x => predefFiles = predefFiles :+ Path(x, pwd) }
        .text("Lets you load your predef from a custom location")
      opt[Unit]('y', "continually")
        .foreach(x => continually = true)
        .text(
          """Lets you run a file over and over, useful for benchmarking purposes
            |since it lets you hook up a profiler to the long-lived process and
            |see where all the time is being spent.
          """.stripMargin)
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
            |interactive REpl
          """.stripMargin)

    }

    val (take, drop) = args0.indexOf("--") match {
      case -1 => (Int.MaxValue, Int.MaxValue)
      case n => (n, n+1)
    }

    val before = args0.take(take)
    val interpArgs = args0.drop(drop).toList
    // Parse interpreter arguments to link to possible script @main entry points
    var keywordTokens = interpArgs
    var kwargs = Vector.empty[(String, String)]

    while(keywordTokens.nonEmpty){
      if (keywordTokens(0).startsWith("--")){
        kwargs = kwargs :+ (keywordTokens(0).drop(2), keywordTokens(1))
        keywordTokens = keywordTokens.drop(2)
      }else{
        passThroughArgs = passThroughArgs :+ keywordTokens(0)
        keywordTokens = keywordTokens.drop(1)
      }
    }
    def ifContinually[T](b: Boolean)(f: => T) = {
      if (b) while(true) f
      else f
    }
    for(c <- replParser.parse(before, Main())) ifContinually(continually){
      def main(isRepl: Boolean) = Main(
        c.predef,
        c.defaultPredef,
        new Storage.Folder(ammoniteHome.getOrElse(Defaults.ammoniteHome), isRepl) {
          override def loadPredef: String = {
            if (predefFiles.isEmpty)
              super.loadPredef
            else
              try {
                predefFiles.map(f => read(f)).mkString("\n")
              } catch {
                case e: java.nio.file.NoSuchFileException => ""
              }
          }
        },
        verboseOutput = verboseOutput
      )
      (fileToExecute, codeToExecute) match {
        case (None, None) => println("Loading..."); main(true).run(interpArgs)
        case (Some(path), None) =>
          main(false).runScript(path, passThroughArgs, kwargs, interpArgs, replApi) match {
            case Res.Failure(exOpt, msg) =>
              Console.err.println(msg)
              System.exit(1)
            case Res.Exception(ex, s) =>
              val trace = ex.getStackTrace
              val i = trace.indexWhere(_.getMethodName == "$main") + 1
              ex.setStackTrace(trace.take(i))
              throw ex
            case Res.Success(_) =>
            // do nothing on success, everything's already happened
          }

        case (None, Some(code)) => main(false).runCode(code, interpArgs, replApi)

      }

    }
  }

  def maybeDefaultPredef(enabled: Boolean, predef: String) =
    if (enabled) predef else ""


}
