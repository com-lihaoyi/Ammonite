package ammonite.main
import acyclic.file
import java.io.{InputStream, InputStreamReader, OutputStream, PrintStream}

import ammonite.frontend._
import ammonite.interp.{History, Interpreter, Storage}
import ammonite.terminal.Filter
import ammonite.util._

import scala.annotation.tailrec

class Repl(input: InputStream,
           output: OutputStream,
           error: OutputStream,
           storage: Storage,
           predef: String,
           wd: ammonite.ops.Path,
           welcomeBanner: Option[String],
           replArgs: Seq[Bind[_]] = Nil) {

  val prompt = Ref("@ ")

  val colors = Ref[Colors](Colors.Default)
  val frontEnd = Ref[FrontEnd](AmmoniteFrontEnd(Filter.empty))

  val printStream = new PrintStream(output, true)
  val errorPrintStream = new PrintStream(error, true)
  var history = new History(Vector())

  def printlnWithColor(color: fansi.Attrs, s: String) = {
    Seq(color(s).render, "\n").foreach(errorPrintStream.print)
  }
  val printer = Printer(
    printStream.print,
    printlnWithColor(colors().warning(), _),
    printlnWithColor(colors().error(), _),
    printlnWithColor(fansi.Attrs.Empty, _)
  )


  val interp: Interpreter = new Interpreter(
    prompt,
    frontEnd,
    frontEnd().width,
    frontEnd().height,
    colors,
    printer,
    storage,
    history,
    predef,
    wd,
    replArgs
  )

  val reader = new InputStreamReader(input)

  def action() = for{
    (code, stmts) <- frontEnd().action(
      input,
      reader,
      output,
      colors().prompt()(prompt()).render,
      colors(),
      interp.pressy.complete(_, interp.replApi.imports, _),
      storage.fullHistory(),
      addHistory = (code) => if (code != "") {
        storage.fullHistory() = storage.fullHistory() :+ code
        history = history :+ code
      }
    )
    _ <- Signaller("INT") { interp.mainThread.stop() }
    out <- interp.processLine(code, stmts, s"cmd${interp.eval.getCurrentLine}.scala")
  } yield {
    printStream.println()
    out
  }

  def run(): Any = {
    welcomeBanner.foreach(printStream.println)
    interp.init()
    @tailrec def loop(): Any = {
      val actionResult = action()
      interp.handleOutput(actionResult)

      actionResult match{
        case Res.Exit(value) =>
          printStream.println("Bye!")
          value
        case Res.Failure(ex, msg) => printer.error(msg)
          loop()
        case Res.Exception(ex, msg) =>
          printer.error(
            Repl.showException(ex, colors().error(), fansi.Attr.Reset, colors().literal())
          )
          printer.error(msg)
          loop()
        case _ =>
          loop()
      }
    }
    loop()
  }
}

object Repl{
  def highlightFrame(f: StackTraceElement,
                     error: fansi.Attrs,
                     highlightError: fansi.Attrs,
                     source: fansi.Attrs) = {
    val src =
      if (f.isNativeMethod) source("Native Method")
      else source(f.getFileName) ++ error(":") ++ source(f.getLineNumber.toString)

    val prefix :+ clsName = f.getClassName.split('.').toSeq
    val prefixString = prefix.map(_+'.').mkString("")
    val clsNameString = clsName //.replace("$", error("$"))
    val method =
      error(prefixString) ++ highlightError(clsNameString) ++ error(".") ++
        highlightError(f.getMethodName)

    fansi.Str(s"  ") ++ method ++ "(" ++ src ++ ")"
  }
  def showException(ex: Throwable,
                    error: fansi.Attrs,
                    highlightError: fansi.Attrs,
                    source: fansi.Attrs) = {
    val cutoff = Set("$main", "evaluatorRunPrinter")
    val traces = Ex.unapplySeq(ex).get.map(exception =>
      error(exception.toString + "\n" +
        exception
          .getStackTrace
          .takeWhile(x => !cutoff(x.getMethodName))
          .map(highlightFrame(_, error, highlightError, source))
          .mkString("\n"))
    )
    traces.mkString("\n")
  }
}