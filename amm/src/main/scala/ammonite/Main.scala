package ammonite

import java.io.{InputStream, OutputStream, PrintStream}
import java.net.URLClassLoader
import java.nio.file.NoSuchFileException

import ammonite.compiler.{CodeClassWrapper, CompilerLifecycleManager, ObjectCodeWrapper}
import ammonite.compiler.iface.CodeWrapper
import ammonite.interp.{Watchable, Interpreter, PredefInitialization, WhiteListClassLoader}
import ammonite.interp.script.AmmoniteBuildServer
import ammonite.runtime.{Frame, Storage}
import ammonite.main._
import ammonite.repl.Repl
import ammonite.util.InterfaceExtensions._
import ammonite.util.Util.{isUnit, newLine}
import ammonite.util._

import scala.annotation.tailrec
import ammonite.runtime.ImportHook
import coursierapi.Dependency



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
                initialColors: String = "default",
                replCodeWrapper: CodeWrapper = ObjectCodeWrapper,
                scriptCodeWrapper: CodeWrapper = ObjectCodeWrapper,
                alreadyLoadedDependencies: Seq[Dependency] =
                  Defaults.alreadyLoadedDependencies(),
                importHooks: Map[Seq[String], ImportHook] = ImportHook.defaults,
                classPathWhitelist: Set[Seq[String]] = Set.empty,
                scalaVersion: Option[String] = None){

  def loadedPredefFile = predefFile match{
    case Some(path) =>
      try Right(Some(PredefInfo(Name("FilePredef"), os.read(path), false, Some(path))))
      catch{case e: NoSuchFileException =>
        Left((
          Res.Failure("Unable to load predef file " + path),
          Seq((Watchable.Path(path),  0L)))
        )
      }
    case None => Right(None)
  }

  lazy val fullScalaVersionOpt: Option[String] = scalaVersion.map { sv =>
    import coursierapi._
    import scala.collection.JavaConverters._

    // FIXME We should allow users to use custom repositories here.

    if (sv.count(_ == '.') >= 2) sv
    else {
      // Short version - we need to find what the exact version is going to be.
      val listing = ammonite.Constants.scalaVersions
      val prefix = sv.stripSuffix(".") + "."
      val matches = listing.filter(v => v.startsWith(prefix))
      matches.lastOption.getOrElse {
        new PrintStream(errorStream).println(s"Error: cannot find a Scala version matching $sv")
        sys.exit(1)
      }
    }
  }

  lazy val extraCp: Array[java.net.URL] = fullScalaVersionOpt match {
    case None => Array.empty
    case Some(sv) =>
      import coursierapi._
      import scala.collection.JavaConverters._
      // FIXME We should allow users to use custom repositories here.
      val files = Fetch.create()
        .withMainArtifacts()
        .addClassifiers("sources")
        .addDependencies(
          Dependency.of("com.lihaoyi", "ammonite-repl-api-full_" + sv, ammonite.Constants.version),
          Dependency.of("com.lihaoyi", "ammonite-compiler_" + sv, ammonite.Constants.version)
        )
        .fetch()
      files
        .iterator()
        .asScala
        .map(_.toURI.toURL)
        .toArray
  }

  def finalWelcomeBanner = welcomeBanner.map { banner =>
    banner.replace(
      "%SCALA%",
      fullScalaVersionOpt.getOrElse(scala.util.Properties.versionNumberString)
    )
  }

  lazy val initialClassLoader: ClassLoader = {
    val contextClassLoader = Thread.currentThread().getContextClassLoader
    new WhiteListClassLoader(classPathWhitelist, contextClassLoader)
  }

  /**
    * Instantiates an ammonite.Repl using the configuration
    */
  def instantiateRepl(replArgs: IndexedSeq[Bind[_]] = Vector.empty) = {


    loadedPredefFile.right.map{ predefFileInfoOpt =>
      val augmentedImports =
        if (defaultPredef) Defaults.replImports ++ Interpreter.predefImports
        else Imports()

      val argString = replArgs.zipWithIndex.map{ case (b, idx) =>
        s"""
        val ${b.name} = ammonite
          .repl
          .ReplBridge
          .value
          .replArgs($idx)
          .asInstanceOf[${b.typeTag.tpe}]
        """
      }.mkString(newLine)

      val frame = {
        // Not passing sharedLoader around where initialClassLoader is used.
        // initialClassLoader is used to compute the "initial class path" later on,
        // that is subject to black/whitelisting. We don't want the JARs of extraCp
        // to get black/whitelisted.
        val sharedLoader =
          if (extraCp.isEmpty) initialClassLoader
          else new URLClassLoader(extraCp, initialClassLoader)
        Frame.createInitial(sharedLoader, forking = extraCp.isEmpty && classPathWhitelist.isEmpty)
      }

      new Repl(
        Interpreter.compilerLifecycleManager(frame.classloader),
        Interpreter.parser(frame.classloader),
        inputStream, outputStream, errorStream,
        storage = storageBackend,
        baseImports = augmentedImports,
        basePredefs = Seq(
          PredefInfo(Name("ArgsPredef"), argString, false, None)
        ),
        customPredefs = predefFileInfoOpt.toSeq ++ Seq(
          PredefInfo(Name("CodePredef"), predefCode, false, Some(wd/"(console)"))
        ),
        wd = wd,
        welcomeBanner = finalWelcomeBanner,
        replArgs = replArgs,
        initialColors = initialColors,
        replCodeWrapper = replCodeWrapper,
        scriptCodeWrapper = scriptCodeWrapper,
        alreadyLoadedDependencies = alreadyLoadedDependencies,
        importHooks = importHooks,
        initialClassLoader = initialClassLoader,
        classPathWhitelist = classPathWhitelist,
        initialFrame = frame
      )
    }

  }

  def instantiateInterpreter() = {
    loadedPredefFile.right.flatMap { predefFileInfoOpt =>
      val augmentedImports =
        if (defaultPredef) Interpreter.predefImports
        else Imports()

      val colors = Ref[ammonite.repl.api.Colors](initialColors match {
        case "b&w" => ammonite.repl.api.Colors.BLACKWHITE
        case _ => ammonite.repl.api.Colors.DEFAULT
      })
      val (_, printer) = Interpreter.initPrinters(
        colors,
        outputStream,
        errorStream,
        verboseOutput
      )

      val frame = {
        val sharedLoader =
          if (extraCp.isEmpty) initialClassLoader
          else new URLClassLoader(extraCp, initialClassLoader)
        Frame.createInitial(sharedLoader, forking = extraCp.isEmpty && classPathWhitelist.isEmpty)
      }

      val customPredefs = predefFileInfoOpt.toSeq ++ Seq(
        PredefInfo(Name("CodePredef"), predefCode, false, None)
      )
      val interp = new Interpreter(
        Interpreter.compilerLifecycleManager(frame.classloader),
        Interpreter.parser(frame.classloader),
        printer,
        storageBackend,
        wd,
        verboseOutput,
        () => frame,
        () => throw new Exception("session loading / saving not possible here"),
        initialClassLoader = initialClassLoader,
        replCodeWrapper,
        scriptCodeWrapper,
        alreadyLoadedDependencies = alreadyLoadedDependencies,
        importHooks = importHooks,
        classPathWhitelist = classPathWhitelist
      )
      val bridges = Nil
      interp.initializePredef(Seq(), customPredefs, bridges, augmentedImports) match{
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
  def run(replArgs: Bind[_]*): (Res[Object], Seq[(Watchable, Long)]) = {


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
          (exitValue.map(repl.beforeExit), repl.interp.watchedValues.toSeq)
        }
    }
  }

  /**
    * Run a Scala script file! takes the path to the file as well as an array
    * of `args` and a map of keyword `kwargs` to pass to that file.
    */
  def runScript(path: os.Path,
                scriptArgs: Seq[String])
                : (Res[Any], Seq[(Watchable, Long)]) = {

    instantiateInterpreter() match{
      case Right(interp) =>
        val result = main.Scripts.runScript(wd, path, interp, scriptArgs)
        (result, interp.watchedValues.toSeq)
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
        (res, interp.watchedValues.toSeq)
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


    val customName = s"Ammonite REPL & Script-Runner, ${ammonite.Constants.version}"
    val customDoc = "usage: amm [ammonite-options] [script-file [script-options]]"
    Config.parser.constructEither(args, customName = customName, customDoc = customDoc) match{
      case Left(msg) =>
        printErr.println(msg)
        false
      case Right(cliConfig) =>
        if (cliConfig.core.bsp.value) {
          val buildServer = new AmmoniteBuildServer(
            defaultCompilerBuilder(),
            defaultParser(),
            ObjectCodeWrapper,
            initialScripts = cliConfig.rest.map(os.Path(_)),
            initialImports = PredefInitialization.initBridges(
              Seq("ammonite.interp.api.InterpBridge" -> "interp")
            ) ++ AmmoniteBuildServer.defaultImports
          )
          val launcher = AmmoniteBuildServer.start(buildServer)
          printErr.println("Starting BSP server")
          val f = launcher.startListening()
          f.get()
          printErr.println("BSP server done")
          // FIXME Doesn't exit for now
          true
        }else{

          val runner = new MainRunner(
            cliConfig, printOut, printErr, stdIn, stdOut, stdErr,
            os.pwd
          )
          (cliConfig.core.code, cliConfig.rest.toList) match{
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


  /**
    * Detects if the console is interactive; lets us make console-friendly output
    * (e.g. ansi color codes) if it is, and script-friendly output (no ansi codes)
    * if it's not
    *
    * https://stackoverflow.com/a/1403817/871202
    */
  def isInteractive() = System.console() != null

  private def defaultCompilerBuilder(): ammonite.compiler.iface.CompilerBuilder =
    ammonite.compiler.Compiler
  private def defaultParser(): ammonite.compiler.iface.Parser =
    ammonite.compiler.Parsers
}

/**
  * Bundles together:
  *
  * - All the code relying on [[cliConfig]]
  * - Handling for the common input/output streams and print-streams
  * - Logic around the watch-and-rerun flag
  */
class MainRunner(cliConfig: Config,
                 outprintStream: PrintStream,
                 errPrintStream: PrintStream,
                 stdIn: InputStream,
                 stdOut: OutputStream,
                 stdErr: OutputStream,
                 wd: os.Path){

  val colors =
    if(cliConfig.core.color.getOrElse(Main.isInteractive())) Colors.Default
    else Colors.BlackWhite

  val initialColors =
    if(cliConfig.core.color.getOrElse(Main.isInteractive())) "default"
    else "b&w"

  def printInfo(s: String) = errPrintStream.println(colors.info()(s))
  def printError(s: String) = errPrintStream.println(colors.error()(s))

  @tailrec final def watchLoop[T](isRepl: Boolean,
                                  printing: Boolean,
                                  run: Main => (Res[T], Seq[(Watchable, Long)])): Boolean = {
    val (result, watched) = run(initMain(isRepl))

    val success = handleWatchRes(result, printing)
    if (!cliConfig.core.watch.value) success
    else{
      watchAndWait(watched)
      watchLoop(isRepl, printing, run)
    }
  }

  def runScript(scriptPath: os.Path, scriptArgs: List[String]) =
    watchLoop(
      isRepl = false,
      printing = true,
      _.runScript(scriptPath, scriptArgs)
    )

  def runCode(code: String) = watchLoop(isRepl = false, printing = false, _.runCode(code))

  def runRepl(): Unit = watchLoop(isRepl = true, printing = false, _.run())

  def watchAndWait(watched: Seq[(Watchable, Long)]) = {
    printInfo(s"Watching for changes to ${watched.length} files... (Ctrl-C to exit)")
    def statAll() = watched.forall{ case (check, lastMTime) =>
      check.poll() == lastMTime
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
        if (printing && !isUnit(value)) outprintStream.println(pprint.PPrinter.BlackWhite(value))
        true

      case Res.Skip   => true // do nothing on success, everything's already happened
    }
    success
  }

  def initMain(isRepl: Boolean) = {
    val storage = if (cliConfig.predef.noHomePredef.value) {
      new Storage.Folder(cliConfig.core.home, isRepl) {
        override def loadPredef = None
      }
    }else{
      new Storage.Folder(cliConfig.core.home, isRepl)
    }

    val codeWrapper =
      if (cliConfig.repl.classBased.value) CodeClassWrapper
      else ObjectCodeWrapper

    Main(
      cliConfig.predef.predefCode,
      cliConfig.core.predefFile,
      !cliConfig.core.noDefaultPredef.value,
      storage,
      wd = wd,
      inputStream = stdIn,
      outputStream = stdOut,
      errorStream = stdErr,
      welcomeBanner = cliConfig.repl.banner match{case "" => None case s => Some(s)},
      verboseOutput = !cliConfig.core.silent.value,
      remoteLogging = !cliConfig.repl.noRemoteLogging.value,
      initialColors = initialColors,
      replCodeWrapper = codeWrapper,
      scriptCodeWrapper = codeWrapper,
      alreadyLoadedDependencies =
        Defaults.alreadyLoadedDependencies(),
      classPathWhitelist =
        if (cliConfig.core.classLoaderIsolation) Repl.getClassPathWhitelist()
        else Set.empty,
      scalaVersion = Some(cliConfig.core.scala).map(_.trim).filter(_.nonEmpty)
    )
  }


}
