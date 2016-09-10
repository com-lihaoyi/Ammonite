package ammonite.kernel

import ammonite.runtime._
import ammonite.util._
//import ammonite.repl._
import kernel._
import scalaz.{Name => _, _}
import Scalaz._
import Validation.FlatMap._

class ReplKernel(storage: Storage, predefs: Seq[(Name, String)]) {

  private val interp: Interpreter = new Interpreter(
    storage,
    predefs
  )

  def process(code: String) = ReplKernel.process(code, interp)

  def complete(text: String, position: Int) = ReplKernel.complete(text, position, interp)

}

object ReplKernel {

  def process(code: String, interp: Interpreter): Option[InterpreterOutput] = {
    val parsed: Option[Validation[LogError, NonEmptyList[String]]] = ParserKernel.parseCode(code)
    parsed map { validation =>
      val validationNel = validation.toValidationNel
      validationNel flatMap { statements =>
        val processed = interp.processLine(statements, s"Main${interp.eval.getCurrentLine}.sc")
        processed foreach (x => interp.handleOutput(x._2))
        processed
      }
    }
  }

  def complete(text: String, position: Int, interp: Interpreter) = {
    interp.pressy.complete(text, position, Munger.importBlock(interp.eval.frames.head.imports))
  }

}
