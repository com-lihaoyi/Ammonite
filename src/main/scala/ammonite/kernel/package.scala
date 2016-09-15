package ammonite.kernel

import scalaz.ValidationNel

package object kernel {

  private[ammonite] type ClassFiles = Vector[(String, Array[Byte])]

  private[ammonite] type SuccessfulCompilation = (List[LogMessage], ClassFiles, Imports)

  private[ammonite] type CompilerOutput = ValidationNel[LogError, SuccessfulCompilation]

  private[ammonite] type SuccessfulExecution = (List[LogMessage], Any)

  type KernelOutput = Option[ValidationNel[LogError, SuccessfulExecution]]

  private[ammonite] val generatedMain = "$main"

  private[ammonite] val newLine = System.lineSeparator()

  private[ammonite] val sessionNameStr = "_sess_"

  private[ammonite] val rootStr = "_root_"

}
