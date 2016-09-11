package ammonite.kernel

import ammonite.util.{Imports, Evaluated}
import scalaz._

package object kernel {

  type ClassFiles = Vector[(String, Array[Byte])]

  type SuccessfulCompilation = (List[LogMessage], ClassFiles, Imports)

  type CompilerOutput = ValidationNel[LogError, SuccessfulCompilation]

  type SuccessfulInterpretation = (List[LogMessage], Evaluated)

  type InterpreterOutput = ValidationNel[LogError, SuccessfulInterpretation]

  type SuccessfulExecution = (List[LogMessage], Any)

  type KernelOutput = Option[ValidationNel[LogError, SuccessfulExecution]]

  protected[ammonite] val previousIden = "_it"

  protected[ammonite] val generatedMain = "$main"

}