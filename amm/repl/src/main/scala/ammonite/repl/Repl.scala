package ammonite.repl

import ammonite.runtime._
import ammonite.util.Util.newLine
import fastparse.core.{Parsed, ParseError}
import ammonite.util._

class ReplKernel(printer: Printer,
                 storage: Storage,
                 predefs: Seq[(Name, String)],
                 wd: ammonite.ops.Path) {

  val prompt = "@ "

  var history = new History(Vector())

  val interp: Interpreter = new Interpreter(
    printer,
    storage,
    predefs,
    i => {
      val replApi = new ReplApiImpl(
        i,
        history,
        new SessionApiImpl(i.eval),
        Vector()
      )
      Seq(("ammonite.repl.ReplBridge", "repl", replApi))
    },
    wd
  )

  def process(code: String) = Parsers.Splitter.parse(code) match {
    case Parsed.Success(statements, _) => 
      val processed = interp.processLine(statements, s"Main${interp.eval.getCurrentLine}.sc")
      interp.handleOutput(processed)
      processed
    case Parsed.Failure(_, index, extra) => 
      Res.Failure(None, ParseError.msg(extra.input, extra.traced.expected, index))
 
  }

}

object Repl {
  val pprintPredef =
    "import ammonite.repl.ReplBridge.value.{pprintConfig, derefPPrint}"

  def highlightFrame(f: StackTraceElement, error: fansi.Attrs, highlightError: fansi.Attrs, source: fansi.Attrs) = {
    val src =
      if (f.isNativeMethod) source("Native Method")
      else
        source(f.getFileName) ++ error(":") ++ source(f.getLineNumber.toString)

    val prefix :+ clsName = f.getClassName.split('.').toSeq
    val prefixString = prefix.map(_ + '.').mkString("")
    val clsNameString = clsName //.replace("$", error("$"))
    val method =
      error(prefixString) ++ highlightError(clsNameString) ++ error(".") ++
        highlightError(f.getMethodName)

    fansi.Str(s"  ") ++ method ++ "(" ++ src ++ ")"
  }
  def showException(ex: Throwable, error: fansi.Attrs, highlightError: fansi.Attrs, source: fansi.Attrs) = {
    val cutoff = Set("$main", "evaluatorRunPrinter")
    val traces = Ex
      .unapplySeq(ex)
      .get
      .map(
        exception =>
          error(
            exception.toString + newLine +
              exception.getStackTrace
                .takeWhile(x => !cutoff(x.getMethodName))
                .map(highlightFrame(_, error, highlightError, source))
                .mkString(newLine)))
    traces.mkString(newLine)
  }
}
