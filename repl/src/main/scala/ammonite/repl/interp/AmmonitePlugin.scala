package ammonite.repl.interp

import ammonite.repl.ImportData


import scala.tools.nsc._
import scala.tools.nsc.plugins.{PluginComponent, Plugin}

/**
 * Used to capture the names in scope after every execution, reporting them
 * to the `output` function. Needs to be a compiler plugin so we can hook in
 * immediately after the `typer`
 */
class AmmonitePlugin(g: scala.tools.nsc.Global, output: Seq[ImportData] => Unit) extends Plugin{
  val name: String = "AmmonitePlugin"
  val global: Global = g
  val description: String = "Extracts the names in scope for the Ammonite REPL to use"
  val components: List[PluginComponent] = List(
    new PluginComponent {
      val global = g
      val runsAfter = List("typer")
      override val runsRightAfter = Some("typer")
      val phaseName = "AmmonitePhase"
      def newPhase(prev: Phase): Phase = new g.GlobalPhase(prev) {
        def name = phaseName
        def apply(unit: g.CompilationUnit): Unit = {
          val stats = unit.body.children.last.asInstanceOf[g.ModuleDef].impl.body
          val symbols = stats.foldLeft(List.empty[(g.Symbol, String)]){
            // These are all the ways we want to import names from previous
            // executions into the current one. Most are straightforward, except
            // `import` statements for which we make use of the typechecker to
            // resolve the imported names
            case (ctx, t @ g.Import(expr, _)) =>
              val syms = new g.analyzer.ImportInfo(t, 0).allImportedSymbols
              syms.map(_ -> expr.toString).toList ::: ctx
            case (ctx, t @ g.DefDef(_, _, _, _, _, _))  => (t.symbol, "") :: ctx
            case (ctx, t @ g.ValDef(_, _, _, _))        => (t.symbol, "") :: ctx
            case (ctx, t @ g.ClassDef(_, _, _, _))      => (t.symbol, "") :: ctx
            case (ctx, t @ g.ModuleDef(_, _, _))        => (t.symbol, "") :: ctx
            case (ctx, t @ g.TypeDef(_, _, _, _))       => (t.symbol, "") :: ctx
            case (ctx, _) => ctx
          }
          output(
            for {
              (sym, importString) <- symbols
              name = sym.encodedName.trim
              if name != "<init>"
              if name != "<clinit>"
              if name != "$main"
            } yield ImportData(sym.encodedName.trim, "", importString)
          )
        }
      }
    }
  )
}