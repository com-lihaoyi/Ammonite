package ammonite.repl

import java.io.{InputStream, InputStreamReader, OutputStream}

import ammonite.runtime._
import ammonite.util.Util.newLine
import ammonite.util._
import scala.annotation.tailrec


import java.io.{InputStream, OutputStream, OutputStreamWriter}

import ammonite.terminal._
import fastparse.core.Parsed
import ammonite.util.Res
import ammonite.runtime.Parsers


class ReplKernel(input: InputStream,
                 output: OutputStream,
                 error: OutputStream,
                 storage: Storage,
                 predef: String,
                 wd: ammonite.ops.Path,
                 replArgs: Seq[Bind[_]]) {

  val prompt = "@ "

  val argString = replArgs.zipWithIndex.map {
    case (b, idx) =>
      s"""
    val ${b.name} =
      ammonite.repl.ReplBridge.value.replArgs($idx).value.asInstanceOf[${b.typeTag.tpe}]
    """
  }.mkString(newLine)

  var history = new History(Vector())

    def frontEnd(input: InputStream,
             reader: java.io.Reader,
             output: OutputStream,
             prompt: String,
             addHistory: String => Unit) = {
    val res = readLine(reader, output, prompt) match {
      case None => Res.Exit(())
      case Some(code) =>
        addHistory(code)
        Parsers.Splitter.parse(code) match {
          case Parsed.Success(value, idx) =>
            Res.Success((code, value))
          case Parsed.Failure(_, index, extra) =>
            Res.Failure(
              None,
              fastparse.core.ParseError.msg(extra.input, extra.traced.expected, index)
            )
        }
    }
    res
  }

  def readLine(reader: java.io.Reader,
               output: OutputStream,
               prompt: String) = {
    val writer = new OutputStreamWriter(output)
    Terminal.readLine(prompt, reader, writer)
  }

  val (colors, printStream, errorPrintStream, printer) =
    Interpreter.initPrinters(output, error, true)

  val interp: Interpreter = new Interpreter(
    printer,
    storage,
    Seq(
      Name("HardcodedPredef") -> Repl.pprintPredef,
      Name("ArgsPredef") -> argString,
      Name("predef") -> predef
    ),
    i => {
      val replApi = new ReplApiImpl(
        i,
        history,
        new SessionApiImpl(i.eval),
        replArgs
      )
      Seq(("ammonite.repl.ReplBridge", "repl", replApi))
    },
    wd
  )

  val reader = new InputStreamReader(input)

  def action() =
    for {
      (code, stmts) <- frontEnd(
        input,
        reader,
        output,
        prompt,
        addHistory = (code) =>
          if (code != "") {
            storage.fullHistory() = storage.fullHistory() :+ code
            history = history :+ code
        }
      )
      _ <- Signaller("INT") { interp.mainThread.stop() }
      out <- interp.processLine(code, stmts, s"cmd${interp.eval.getCurrentLine}.sc")
    } yield {
      printStream.println()
      out
    }

}

class Repl(input: InputStream,
           output: OutputStream,
           error: OutputStream,
           storage: Storage,
           predef: String,
           wd: ammonite.ops.Path,
           welcomeBanner: Option[String],
           replArgs: Seq[Bind[_]] = Nil)
    extends ReplKernel(input, output, error, storage, predef, wd, replArgs) {

  // def run(): Any = {
  //   welcomeBanner.foreach(printStream.println)
  //   interp.init()
  //   @tailrec def loop(): Any = {
  //     val actionResult = action()
  //     interp.handleOutput(actionResult)

  //     actionResult match {
  //       case Res.Exit(value) =>
  //         printStream.println("Bye!")
  //         value
  //       case Res.Failure(ex, msg) =>
  //         printer.error(msg)
  //         loop()
  //       case Res.Exception(ex, msg) =>
  //         printer.error(
  //           Repl.showException(ex, colors().error(), fansi.Attr.Reset, colors().literal())
  //         )
  //         printer.error(msg)
  //         loop()
  //       case _ =>
  //         loop()
  //     }
  //   }
  //   loop()
  // }
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
