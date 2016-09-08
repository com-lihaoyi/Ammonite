package ammonite.kernel

import ammonite.runtime._
import fastparse.core.{Parsed, ParseError}
import ammonite.util._
import ammonite.repl._

class ReplKernel(printer: PrinterX, storage: Storage, predefs: Seq[(Name, String)], wd: ammonite.ops.Path) {

  private val interp: Interpreter = new Interpreter(
    printer,
    storage,
    predefs,
    i => {
      val replApi = new ReplApiImpl(i)
      Seq(("ammonite.repl.ReplBridge", "repl", replApi))
    },
    wd
  )

  def process(code: String) = ReplKernel.process(code, interp)

  def complete(text: String, position: Int) = ReplKernel.complete(text, position, interp)

}

object ReplKernel{

  def process(code: String, interp: Interpreter) = Parsers.Splitter.parse(code) match {
    case Parsed.Success(statements, _) =>
      val processed =
        interp.processLine(statements, s"Main${interp.eval.getCurrentLine}.sc")
      interp.handleOutput(processed)
      processed
    case Parsed.Failure(_, index, extra) =>
      Res.Failure(None, ParseError.msg(extra.input, extra.traced.expected, index))
  }

  def complete(text: String, position: Int, interp: Interpreter) = {
    interp.pressy.complete(text, position, Preprocessor.importBlock(interp.eval.frames.head.imports))
  }

}
