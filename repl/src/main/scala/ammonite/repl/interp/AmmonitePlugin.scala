package ammonite.repl.interp

import ammonite.repl.{BacktickWrap, ImportData}
import acyclic.file

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
        def decode(t: g.Tree) = {
          val sym = t.symbol
          (sym, sym.decodedName, sym.decodedName, "")
        }
        def apply(unit: g.CompilationUnit): Unit = {
          val stats = unit.body.children.last match {
            case m: g.ModuleDef => m.impl.body
            case c: g.ClassDef => c.impl.body
            case other => throw new IllegalArgumentException(s"Unsupported wrapper definition: $other")
          }
          val symbols = stats.foldLeft(List.empty[(g.Symbol, String, String, String)]){
            // These are all the ways we want to import names from previous
            // executions into the current one. Most are straightforward, except
            // `import` statements for which we make use of the typechecker to
            // resolve the imported names
            case (ctx, t @ g.Import(expr, selectors)) =>
              def rec(expr: g.Tree): List[g.Name] = {
                expr match {
                  case g.Select(lhs, name) => name :: rec(lhs)
                  case g.Ident(name) => List(name)
                }
              }
              val prefix = rec(expr).reverse
                .map(x => BacktickWrap(x.decoded))
                .mkString(".")

              val renamings =
                for(t @ g.ImportSelector(name, _, rename, _) <- selectors) yield {
                  Option(rename).map(name.decoded -> _.decoded)
                }
              val renameMap = renamings.flatten.map(_.swap).toMap
              val info = new g.analyzer.ImportInfo(t, 0)

              val syms = for{
                sym <- info.allImportedSymbols.toList
                if sym.isPublic
              } yield {
                val name = sym.decodedName
                (sym, renameMap.getOrElse(name, name), name, prefix)
              }

              syms ::: ctx
            case (ctx, t @ g.DefDef(_, _, _, _, _, _))  => decode(t) :: ctx
            case (ctx, t @ g.ValDef(_, _, _, _))        => decode(t) :: ctx
            case (ctx, t @ g.ClassDef(_, _, _, _))      => decode(t) :: ctx
            case (ctx, t @ g.ModuleDef(_, _, _))        => decode(t) :: ctx
            case (ctx, t @ g.TypeDef(_, _, _, _))       => decode(t) :: ctx
            case (ctx, _) => ctx
          }

          output(
            for {
              (sym, fromName, toName, importString) <- symbols
//              _ = println(fromName + "\t"+ toName)
              if !sym.isSynthetic
              if !sym.isPrivate
              if !fromName.contains("$")
              if fromName != "<init>"
              if fromName != "<clinit>"
              if fromName != "$main"
            } yield ImportData(fromName, toName, "", importString)
          )
        }
      }
    }
  )
}
