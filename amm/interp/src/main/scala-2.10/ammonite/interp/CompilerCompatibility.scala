package ammonite.interp

import scala.tools.nsc.Global
import scala.tools.nsc.interactive.{ Global => InteractiveGlobal }
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.typechecker.Analyzer

object CompilerCompatibility {
  def newUnitParser(compiler: Global, line: String, fileName: String) = {
    // Can't pass in the fileName in Scala 2.10
    compiler.newUnitParser(line)
  }
  def analyzer(g: Global, cl: ClassLoader): Analyzer { val global: g.type } =
    new { val global: g.type = g } with Analyzer {
      override lazy val macroClassloader = cl
    }

  type InteractiveAnalyzer = Analyzer

  def interactiveAnalyzer(g: InteractiveGlobal, cl: ClassLoader)
                         : InteractiveAnalyzer { val global: g.type } = {
    analyzer(g, cl)
  }
  def trees(g: Global)(parser: g.syntaxAnalyzer.UnitParser): Seq[Global#Tree] =
    parser.templateStats() ++ parser.topStatSeq()

  def pluginInit(plugin: Plugin, options: List[String], error: String => Unit): Boolean = {
    plugin.processOptions(options, error)
    true
  }

}
