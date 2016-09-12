package ammonite.kernel

import ammonite.runtime._
import kernel._
import scalaz.{Name => _, _}
import Scalaz._
import Validation.FlatMap._
import ammonite.runtime.Parsers
import fastparse.core.{Parsed, ParseError}
import ammonite.util.Imports

class ReplKernel private (private[this] var state: ReplKernel.KernelState) {

  private val interp: Interpreter = new Interpreter()

  def process(code: String): KernelOutput = {

    val evaluationIndex = state.evaluationIndex

    val parsed = Parsers.Splitter.parse(code) match {
      case Parsed.Success(statements, _) =>
        statements.toList match {
          case h :: t =>
            val nel = NonEmptyList(h, t: _*)
            Some(Validation.success(nel))
          case Nil => None
        }
      case Parsed.Failure(_, index, extra) =>
        Some(Validation.failure(LogError(ParseError.msg(extra.input, extra.traced.expected, index))))
    }

    val withReversedErrors = parsed map { validation =>
      val validationNel = validation.toValidationNel
      validationNel flatMap { statements =>
        val processed = interp.processLine(statements, s"_ReplKernel$evaluationIndex.sc", evaluationIndex)
        processed map {
          case (logMessages, evaluated) => (logMessages.reverse, evaluated.value)
        }
      }
    }

    val res = withReversedErrors.map(_.leftMap(_.reverse))

    res match {
      case Some(Success(_)) => state = state.copy(evaluationIndex = state.evaluationIndex + 1)
      case _ => ()
    }
    res
  }

  def complete(text: String, position: Int) = {
    interp.pressy.complete(text, position, Munger.importBlock(interp.frame.imports))
  }

}

object ReplKernel {

  private case class KernelState(evaluationIndex: Int, frame: Frame)

  def apply(): ReplKernel = {
    val frame = {
      val currentClassLoader = Thread.currentThread().getContextClassLoader
      val hash = SpecialClassLoader.initialClasspathSignature(currentClassLoader)
      def special = new SpecialClassLoader(currentClassLoader, hash)
      new Frame(special, special, Imports(), Seq())
    }
    new ReplKernel(KernelState(0, frame))
  }

}
