package ammonite.repl

import java.io.{PrintStream, InputStream, OutputStream, InputStreamReader}
import ammonite.repl.frontend._
import acyclic.file
import ammonite.repl.interp.Interpreter

import scala.annotation.tailrec
class Repl(input: InputStream,
           output: OutputStream,
           error: OutputStream,
           storage: Ref[Storage],
           predef: String = "",
           replArgs: Seq[Bind[_]] = Nil) {

  val prompt = Ref("@ ")

  val colors = Ref[Colors](Colors.Default)
  val frontEnd = Ref[FrontEnd](AmmoniteFrontEnd(
    PartialFunction.empty
  ))

  val printStream = new PrintStream(output, true)
  val errorPrintStream = new PrintStream(error, true)
  var history = new History(Vector())


  def printlnWithColor(color: String, s: String) = {
    Seq(color, s, colors().reset(), "\n").foreach(errorPrintStream.print)
  }
  val printer = Printer(
    _.foreach(printStream.print),
    printlnWithColor(colors().warning(), _),
    printlnWithColor(colors().error(), _)
  )
  Timer("Repl init printer")

  
  val interp: Interpreter = new Interpreter(
    prompt,
    frontEnd,
    frontEnd().width,
    frontEnd().height,
    pprint.Config.Colors.PPrintConfig,
    colors,
    printer,
    storage,
    history,
    predef,
    replArgs
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
    out <- interp.processLine(code, stmts)
  } yield {
    Timer("interp.processLine end")
    printStream.println()
    out
  }

  def ammoniteVersion = ammonite.Constants.version
  def scalaVersion = scala.util.Properties.versionNumberString
  def javaVersion = System.getProperty("java.version")

  def printBanner(): Unit = {
    printStream.println(s"Welcome to the Ammonite Repl $ammoniteVersion")
    printStream.println(s"(Scala $scalaVersion Java $javaVersion)")
  }

  def run(): Any = {
    printBanner()
    @tailrec def loop(): Any = {
      val res = action()
      Timer("End Of Loop")
      val res2 = res match{
        case Res.Exit(value) =>
          printStream.println("Bye!")
          value
        case Res.Failure(msg) => printer.error(msg)
        case Res.Exception(ex, msg) =>
          printer.error(
            Repl.showException(ex, colors().error(), colors().reset(), colors().literal())
          )
          printer.error(msg)
        case _ =>
      }

      if (interp.handleOutput(res)) loop()
      else res2
    }
    loop()
  }
}

object Repl{
  def highlightFrame(f: StackTraceElement,
                     error: String,
                     highlightError: String,
                     source: String) = {
    val src =
      if (f.isNativeMethod) source + "Native Method" + error
      else s"$source${f.getFileName}$error:$source${f.getLineNumber}$error"

    val prefix :+ clsName = f.getClassName.split('.').toSeq
    val prefixString = prefix.map(_+'.').mkString("")
    val clsNameString = clsName.replace("$", error+"$"+highlightError)
    val method =
      s"$error$prefixString$highlightError$clsNameString$error" +
        s".$highlightError${f.getMethodName}$error"

    s"  $method($src)"
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
}