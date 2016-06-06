package ammonite.repl

import java.io.{PrintStream, InputStream, OutputStream, InputStreamReader}
import ammonite.repl.frontend._
import acyclic.file
import ammonite.repl.interp.Interpreter
import ammonite.terminal.Filter

import scala.annotation.tailrec
class Repl(input: InputStream,
           output: OutputStream,
           error: OutputStream,
           storage: Storage,
           wd: ammonite.ops.Path,
           welcomeBanner: Option[String],
           replArgs: Seq[Bind[_]] = Nil,
           classOutputDir: Option[String] = None) {

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
    _.foreach(printStream.print),
    printlnWithColor(colors().warning(), _),
    printlnWithColor(colors().error(), _),
    printlnWithColor(fansi.Attrs.Empty, _)
  )
  Timer("Repl init printer")


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
    replArgs,
    classOutputDir = classOutputDir
  )

  Timer("Repl init interpreter")
  val reader = new InputStreamReader(input)

  def action() = for{
    (code, stmts) <- frontEnd().action(
      input,
      reader,
      output,
      colors().prompt()(prompt()).render,
      colors(),
      interp.pressy.complete(_, interp.eval.previousImportBlock, _),
      storage.fullHistory(),
      addHistory = (code) => if (code != "") {
        storage.fullHistory() = storage.fullHistory() :+ code
        history = history :+ code
      }
    )
    _ <- Signaller("INT") { interp.mainThread.stop() }
    out <- interp.processLine(code, stmts, s"cmd${interp.eval.getCurrentLine}.scala")
  } yield {
    Timer("interp.processLine end")
    printStream.println()
    out
  }

  def run(): Any = {
    welcomeBanner.foreach(printStream.println)
    @tailrec def loop(): Any = {
      val res = action()
      Timer("End Of Loop")
      val res2 = res match{
        case Res.Exit(value) =>
          printStream.println("Bye!")
          value
        case Res.Failure(ex, msg) => printer.error(msg)
        case Res.Exception(ex, msg) =>
          printer.error(
            Repl.showException(ex, colors().error(), fansi.Attr.Reset, colors().literal())
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
