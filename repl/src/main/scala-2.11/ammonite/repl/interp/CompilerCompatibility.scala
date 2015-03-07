package ammonite.repl.interp

import scala.tools.nsc.Global
import scala.tools.nsc.interactive.{ Global => InteractiveGlobal }
import scala.tools.nsc.typechecker.Analyzer

object CompilerCompatibility {
  def analyzer(g: Global): Analyzer { val global: g.type } =
    new { val global: g.type = g } with Analyzer {
      override def findMacroClassLoader() = new ClassLoader(this.getClass.getClassLoader){}
    }

  type InteractiveAnalyzer = scala.tools.nsc.interactive.InteractiveAnalyzer

  def interactiveAnalyzer(g: InteractiveGlobal): InteractiveAnalyzer { val global: g.type } =
    new { val global: g.type = g } with InteractiveAnalyzer {
      override def findMacroClassLoader() = new ClassLoader(this.getClass.getClassLoader) {}
    }

  def trees(g: Global)(parser: g.syntaxAnalyzer.UnitParser): Seq[Global#Tree] =
    parser.parseStatsOrPackages()
}
