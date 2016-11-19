package ammonite.runtime

import ammonite.util.ImportData

import scala.reflect.internal.util.Position
import scala.reflect.io
import scala.reflect.io.{AbstractFile, FileZipArchive, VirtualDirectory}
import scala.tools.nsc.classpath._
import scala.tools.nsc.{Global, Settings}
import scala.tools.nsc.interactive.{Global => InteractiveGlobal}
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.util.ClassPath.JavaContext
import scala.tools.nsc.util._
import scala.reflect.io._
import scala.tools.nsc
import scala.tools.nsc.backend.JavaPlatform
object CompilerCompatibility {
  def analyzer(g: Global, cl: ClassLoader): Analyzer { val global: g.type } =
    new { val global: g.type = g } with Analyzer {
      override def findMacroClassLoader() = cl
    }

  type InteractiveAnalyzer = scala.tools.nsc.interactive.InteractiveAnalyzer

  def interactiveAnalyzer(g: InteractiveGlobal, cl: ClassLoader)
                         : InteractiveAnalyzer { val global: g.type } =
    new { val global: g.type = g } with InteractiveAnalyzer {
      override def findMacroClassLoader() = cl
    }

  def trees(g: Global)(parser: g.syntaxAnalyzer.UnitParser): Seq[Global#Tree] =
    parser.parseStatsOrPackages()

  def pluginInit(plugin: Plugin, options: List[String], error: String => Unit): Boolean =
    plugin.init(options, error)

}
