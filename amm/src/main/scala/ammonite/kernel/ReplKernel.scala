package ammonite.kernel

import ammonite.runtime._
import fastparse.core.{Parsed, ParseError}
import ammonite.util._
import ammonite.repl._

class ReplKernel(printer: PrinterX, storage: Storage, predefs: Seq[(Name, String)], wd: ammonite.ops.Path) {

  var history = new History(Vector())

  val interp: Interpreter = new Interpreter(
    printer,
    storage,
    predefs,
    i => {
      val replApi = new ReplApiImpl(
        i,
        history,
        new SessionApiImpl(i.eval)
      )
      Seq(("ammonite.repl.ReplBridge", "repl", replApi))
    },
    wd
  )

  def process(code: String) = Parsers.Splitter.parse(code) match {
    case Parsed.Success(statements, _) =>
      val processed =
        interp.processLine(statements, s"Main${interp.eval.getCurrentLine}.sc")
      interp.handleOutput(processed)
      processed
    case Parsed.Failure(_, index, extra) =>
      Res.Failure(None, ParseError.msg(extra.input, extra.traced.expected, index))
  }

  def complete(text: String, position: Int) = {
    interp.pressy.complete(text, position, Preprocessor.importBlock(interp.eval.frames.head.imports))
  }

}
