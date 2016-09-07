package ammonite.repl

import ammonite.util.Util.newLine
import ammonite.util._

object Repl {
  val pprintPredef =
    "import ammonite.repl.ReplBridge.value.{pprintConfig, derefPPrint}"

  def highlightFrame(f: StackTraceElement,
                     error: fansi.Attrs,
                     highlightError: fansi.Attrs,
                     source: fansi.Attrs) = {
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
  def showException(ex: Throwable,
                    error: fansi.Attrs,
                    highlightError: fansi.Attrs,
                    source: fansi.Attrs) = {
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
