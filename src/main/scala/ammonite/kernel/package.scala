package ammonite.kernel

import ammonite.util.Imports
import scalaz._

package object kernel {

  type ClassFiles = Vector[(String, Array[Byte])]

  type SuccessfulCompilation = (List[LogMessage], ClassFiles, Imports)

  type CompilerOutput = ValidationNel[LogError, SuccessfulCompilation]

  type SuccessfulExecution = (List[LogMessage], Any)

  type KernelOutput = Option[ValidationNel[LogError, SuccessfulExecution]]

  val generatedMain = "$main"

}