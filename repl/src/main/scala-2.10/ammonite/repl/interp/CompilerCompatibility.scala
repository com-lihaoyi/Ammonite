package ammonite.repl.interp

import scala.tools.nsc.Global
import scala.tools.nsc.interactive.{ Global => InteractiveGlobal }
import scala.tools.nsc.typechecker.Analyzer

object CompilerCompatibility {
  def analyzer(g: Global): Analyzer { val global: g.type } =
    new { val global: g.type = g } with Analyzer {
      override lazy val macroClassloader = new ClassLoader(this.getClass.getClassLoader) {}
    }

  type InteractiveAnalyzer = Analyzer

  def interactiveAnalyzer(g: InteractiveGlobal): InteractiveAnalyzer { val global: g.type } =
    analyzer(g)

  def trees(g: Global)(parser: g.syntaxAnalyzer.UnitParser): Seq[Global#Tree] =
    parser.templateStats() ++ parser.topStatSeq()
}
