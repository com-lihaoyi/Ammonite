package ammonite.compiler.iface

import java.nio.file.Path

import ammonite.util.{Frame, Printer}
import ammonite.util.Util.ClassFiles

abstract class CompilerLifecycleManager {
  def compiler: Compiler
  def compilationCount: Int

  def preprocess(fileName: String): Preprocessor

  def scalaVersion: String

  def outputDir: Option[Path]

  def init(force: Boolean = false): Unit

  def complete(
      offset: Int,
      previousImports: String,
      snippet: String
  ): (Int, Seq[String], Seq[String])

  def compileClass(
      processed: Preprocessor.Output,
      printer: Printer,
      fileName: String
  ): Option[Compiler.Output]

  def addToClasspath(classFiles: ClassFiles): Unit

  def shutdownPressy(): Unit
}
