package ammonite.kernel

import ammonite.util.{Imports, Evaluated}
import scalaz._

package object kernel {

  type ClassFiles = Vector[(String, Array[Byte])]

  type SuccessfulCompilation = (List[LogMessage], ClassFiles, Imports)

  type CompilerOutput = ValidationNel[LogError, SuccessfulCompilation]

  type SuccessfulInterpretation = (List[LogMessage], Evaluated)

  type InterpreterOutput = ValidationNel[LogError, SuccessfulInterpretation]

  protected[ammonite] val previousIden = "_id"

  protected[ammonite] val generatedMain = "$main"

}