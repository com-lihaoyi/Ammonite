package ammonite.repl

import java.io._
import ammonite.Constants
import ammonite.repl.Util.IvyMap
import ammonite.repl.frontend._
import acyclic.file
import ammonite.repl.interp.Interpreter
import Util.CompileCache

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try
import ammonite.ops._
class Repl(input: InputStream,
           output: OutputStream,
           storage: Ref[Storage],
           predef: String = "") {

  val prompt = Ref("@ ")

  val colors = Ref[Colors](Colors.Default)
  val frontEnd = Ref[FrontEnd](FrontEnd.Ammonite)

  val printer = new PrintStream(output, true)
  var history = new History(Vector())

  Timer("Repl init printer")
  val interp: Interpreter = new Interpreter(
    prompt,
    frontEnd,
    frontEnd().width,
    frontEnd().height,
    pprint.Config.Colors.PPrintConfig,
    colors,
    printer.print,
    storage,
    history,
    predef
  )

  Timer("Repl init interpreter")
  val reader = new InputStreamReader(input)
  def action() = for{
    (code, stmts) <- frontEnd().action(
      input,
      reader,
      output,
      colors().prompt() + prompt() + colors().reset(),
      colors(),
      interp.pressy.complete(_, interp.eval.previousImportBlock, _),
      storage().fullHistory(),
      addHistory = (code) => if (code != "") {
        storage().fullHistory() = storage().fullHistory() :+ code
        history = history :+ code
      }
    )
    _ <- Signaller("INT") { interp.mainThread.stop() }
    out <- interp.processLine(code, stmts, _.foreach(printer.print))
  } yield {
    Timer("interp.processLine end")
    printer.println()
    out
  }

  def ammoniteVersion: String =
    Constants.version

  def scalaVersion: String =
    scala.util.Properties.versionString

  def javaVersion: String =
    System.getProperty("java.version")

  def printBanner(): Unit = {
    printer.println(s"Welcome to the Ammonite Repl $ammoniteVersion")
    printer.println(s"(Scala $scalaVersion Java $javaVersion)")
  }

  def run() = {
    printBanner()
    @tailrec def loop(): Unit = {
      val res = action()
      res match{
        case Res.Exit => printer.println("Bye!")
        case Res.Failure(msg) => printer.println(colors().error() + msg + colors().reset())
        case Res.Exception(ex, msg) =>
          printer.println(
            Repl.showException(ex, colors().error(), colors().reset(), colors().literal())
          )
          printer.println(colors().error() + msg + colors().reset())
        case _ =>
      }
      Timer("End Of Loop")
      if (interp.handleOutput(res)) loop()
    }
    loop()
  }
}

object Repl{
  def highlightFrame(f: StackTraceElement, error: String, highlightError: String, source: String) = {
    val src =
      if (f.isNativeMethod) source + "Native Method" + error
      else s"$source${f.getFileName}$error:$source${f.getLineNumber}$error"

    val prefix :+ clsName = f.getClassName.split('.').toSeq
    val prefixString = prefix.map(_+'.').mkString("")
    val clsNameString = clsName.replace("$", error+"$"+highlightError)
    val method = s"$error$prefixString$highlightError$clsNameString$error.$highlightError${f.getMethodName}$error"
    s"\t$method($src)"
  }
  def showException(ex: Throwable, error: String, highlightError: String, source: String) = {
    val cutoff = Set("$main", "evaluatorRunPrinter")
    val traces = Ex.unapplySeq(ex).get.map(exception =>
      error + exception.toString + "\n" +
        exception
          .getStackTrace
          .takeWhile(x => !cutoff(x.getMethodName))
          .map(highlightFrame(_, error, highlightError, source))
          .mkString("\n")
    )
    traces.mkString("\n")
  }
  case class Config(predef: String = "",
                    ammoniteHome: Path = defaultAmmoniteHome,
                    file: Option[Path] = None)

  def defaultAmmoniteHome = Path(System.getProperty("user.home"))/".ammonite"
  def main(args: Array[String]) = {
    val parser = new scopt.OptionParser[Config]("ammonite") {
      head("ammonite", Constants.version)
      opt[String]('p', "predef")
        .action((x, c) => c.copy(predef = x))
        .text("Any commands you want to execute at the start of the REPL session")
      opt[File]('h', "home")
        .valueName("<file>")
        .action((x, c) => c.copy(ammoniteHome = Path(x)))
        .text("The home directory of the REPL; where it looks for config and caches")
      arg[File]("<file>...")
        .optional()
        .action { (x, c) => c.copy(file = Some(Path(x))) }
        .text("The Ammonite script file you want to execute")
    }
    parser.parse(args, Config()).foreach(c => run(c.predef, c.ammoniteHome, c.file))
  }
  def run(predef: String = "",
          ammoniteHome: Path = defaultAmmoniteHome,
          file: Option[Path] = None) = {

    Timer("Repl.run Start")
    def storage = Storage(ammoniteHome)
    def repl = new Repl(
      System.in, System.out,
      storage = Ref(storage),
      predef = predef + "\n" + storage.loadPredef
    )
    file match{
      case None =>
        println("Loading...")
        repl.run()
      case Some(path) =>
        repl.interp.replApi.load.module(path)
    }
    Timer("Repl.run End")
  }
}

