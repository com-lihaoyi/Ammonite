package ammonite

import java.io.{InputStream, OutputStream, PrintStream}
import java.net.URLClassLoader
import java.nio.file.NoSuchFileException

import ammonite.compiler.iface.{CodeWrapper, CompilerBuilder, Parser}
import ammonite.compiler.{CodeClassWrapper, DefaultCodeWrapper}
import ammonite.interp.script.AmmoniteBuildServer
import ammonite.interp.{Interpreter, PredefInitialization, Watchable}
import ammonite.main._
import ammonite.runtime.{Frame, ImportHook, Storage}
import ammonite.util.Util.newLine
import ammonite.util._
import coursierapi.Dependency
import ammonite.repl.Repl
import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration.Duration

// needed to support deprecated Main.main
import acyclic.skipped

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

  // for trapping exit when the --watch option is on
  if (cliConfig.core.watch.value && ammonite.util.Util.javaMajorVersion < 17)
    System.setSecurityManager(TrapExitSecurityManager)

  val colors =
    if(cliConfig.core.color.getOrElse(ammonite.util.Util.isInteractive())) Colors.Default
    else Colors.BlackWhite

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
    val watchedPaths = watched.count {
      case (ammonite.interp.Watchable.Path(p), _) => true
      case (_, _) => false
    }
    val watchedValues = watched.size - watchedPaths

    val watchedValueStr = if (watchedValues == 0) "" else s" and $watchedValues other values"

    printInfo(
      s"Watching for changes to $watchedPaths paths$watchedValueStr... (Enter to re-run, Ctrl-C to exit)"
    )

    MainRunner.statWatchWait(watched, stdIn)
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
    val storage = if (cliConfig.predef.noHomePredef.value) {
      new Storage.Folder(cliConfig.core.home, isRepl) {
        override def loadPredef = None
      }
    }else{
      new Storage.Folder(cliConfig.core.home, isRepl)
    }

    lazy val parser = ammonite.compiler.Parsers
    val codeWrapper =
      if (cliConfig.repl.classBased.value) CodeClassWrapper
      else DefaultCodeWrapper

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
      colors = colors,
      replCodeWrapper = codeWrapper,
      scriptCodeWrapper = codeWrapper,
      parser = () => parser,
      alreadyLoadedDependencies =
        Defaults.alreadyLoadedDependencies(),
      classPathWhitelist = ammonite.repl.Repl.getClassPathWhitelist(cliConfig.core.thin.value)

    )
  }

}
object MainRunner{

  /**
   * Polls for updates until either one of the input files changes,
   * or the enter key is pressed
   * */
  def statWatchWait(watched: Seq[(Watchable, Long)],
                    stdIn: InputStream): Unit = {
    val buffer = new Array[Byte](4 * 1024)

    def allWatchedUnchanged() =
      watched.forall { case (file, lastMTime) => file.poll() == lastMTime }

    @tailrec def statWatchWait0(): Unit = {
      if (allWatchedUnchanged()) {
        if (lookForEnterKey()) ()
        else {
          Thread.sleep(100)
          statWatchWait0()
        }
      }
    }
    @tailrec def lookForEnterKey(): Boolean = {
      if (stdIn.available() == 0) false
      else stdIn.read(buffer)  match{
        case 0 | -1 => false
        case n =>
          buffer.indexOf('\n') match{
            case -1 => lookForEnterKey()
            case i =>
              if (i >= n) lookForEnterKey()
              else true
          }
      }
    }
    statWatchWait0()
  }


}