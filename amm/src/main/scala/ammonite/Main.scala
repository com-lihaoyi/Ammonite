package ammonite

import java.io.{InputStream, OutputStream, PrintStream}
import java.net.URLClassLoader
import java.nio.file.NoSuchFileException

import ammonite.compiler.{CodeClassWrapper, DefaultCodeWrapper}
import ammonite.compiler.iface.{CodeWrapper, CompilerBuilder, Parser}
import ammonite.interp.{Watchable, Interpreter, PredefInitialization}
import ammonite.interp.script.AmmoniteBuildServer
import ammonite.runtime.{Frame, Storage}
import ammonite.main._
import ammonite.repl.{FrontEndAPIImpl, Repl}
import ammonite.util.Util.newLine
import ammonite.util._

import scala.annotation.tailrec
import ammonite.runtime.ImportHook
import coursierapi.Dependency
import scala.concurrent.Await
import scala.concurrent.duration.Duration

// needed to support deprecated Main.main
import acyclic.skipped


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
                @deprecated("remoteLogging has been removed, do not use this field",
                            "Ammonite 2.3.0")
                remoteLogging: Boolean = true,
                colors: Colors = Colors.Default,
                replCodeWrapper: CodeWrapper = DefaultCodeWrapper,
                scriptCodeWrapper: CodeWrapper = DefaultCodeWrapper,
                alreadyLoadedDependencies: Seq[Dependency] =
                  Defaults.alreadyLoadedDependencies(),
                importHooks: Map[Seq[String], ImportHook] = ImportHook.defaults,
                compilerBuilder: CompilerBuilder = ammonite.compiler.CompilerBuilder(),
                // by-name, so that fastparse isn't loaded when we don't need it
                parser: () => Parser = () => ammonite.compiler.Parsers,
                classPathWhitelist: Set[Seq[String]] = Set.empty,
                warnings: Boolean = false){

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

  def initialClassLoader: ClassLoader = {
    val contextClassLoader = Thread.currentThread().getContextClassLoader
    new ammonite.util.WhiteListClassLoader(classPathWhitelist, contextClassLoader)
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
          .Internal
          .replArgs($idx)
          .value
          .asInstanceOf[${b.typeTag.tpe}]
        """
      }.mkString(newLine)

      new Repl(
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
        welcomeBanner = welcomeBanner,
        replArgs = replArgs,
        initialColors = colors,
        replCodeWrapper = replCodeWrapper,
        scriptCodeWrapper = scriptCodeWrapper,
        alreadyLoadedDependencies = alreadyLoadedDependencies,
        importHooks = importHooks,
        compilerBuilder = compilerBuilder,
        parser = parser(),
        initialClassLoader = initialClassLoader,
        classPathWhitelist = classPathWhitelist,
        warnings = warnings
      )
    }

  }

  def instantiateInterpreter() = {
    loadedPredefFile.right.flatMap { predefFileInfoOpt =>
      val augmentedImports =
        if (defaultPredef) Interpreter.predefImports
        else Imports()

      val (colorsRef, printer) = Interpreter.initPrinters(
        colors,
        outputStream,
        errorStream,
        verboseOutput
      )
      val frame = Frame.createInitial(initialClassLoader)

      val customPredefs = predefFileInfoOpt.toSeq ++ Seq(
        PredefInfo(Name("CodePredef"), predefCode, false, None)
      )
      lazy val parser0 = parser()
      val interpParams = Interpreter.Parameters(
        printer = printer,
        storage = storageBackend,
        wd = wd,
        colors = colorsRef,
        verboseOutput = verboseOutput,
        initialClassLoader = initialClassLoader,
        importHooks = importHooks,
        classPathWhitelist = classPathWhitelist,
        alreadyLoadedDependencies = alreadyLoadedDependencies,
        warnings = warnings
      )
      val interp = new Interpreter(
        compilerBuilder,
        () => parser0,
        () => frame,
        () => throw new Exception("session loading / saving not possible here"),
        replCodeWrapper,
        scriptCodeWrapper,
        parameters = interpParams
      )
      val bridges = Seq(
        (
          "ammonite.repl.api.FrontEndBridge",
          "frontEnd",
          new FrontEndAPIImpl {
            def parser = parser0
          }
        )
      )
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
  def run(replArgs: Bind[_]*): (Res[Any], Seq[(Watchable, Long)]) = {


    instantiateRepl(replArgs.toIndexedSeq) match{
      case Left(missingPredefInfo) => missingPredefInfo
      case Right(repl) =>
        repl.initializePredef() match {
          case Some((e: Res.Exception, _)) =>
            // just let exceptions during predef propagate up
            throw e.t
          case Some(value) =>
            value
          case None =>
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

object Main {
  @deprecated("Use ammonite.AmmoniteMain.main instead", "2.5.0")
  def main(args: Array[String]): Unit = AmmoniteMain.main(args)
}
