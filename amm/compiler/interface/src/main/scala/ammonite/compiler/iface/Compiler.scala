package ammonite.compiler.iface

import ammonite.util.{Imports, Printer}

abstract class Compiler {

  def compile(
    src: Array[Byte],
    printer: Printer,
    importsLen: Int,
    userCodeNestingLevel: Int,
    fileName: String
  ): Option[Compiler.Output]

  def preprocessor(fileName: String, markGeneratedSections: Boolean = false): Preprocessor

}

object Compiler {

  case class Output(
    classFiles: Vector[(String, Array[Byte])],
    imports: Imports,
    usedEarlierDefinitions: Option[Seq[String]]
  )

}

