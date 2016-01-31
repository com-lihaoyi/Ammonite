package ammonite.repl.interp

import ammonite.repl.{Parsers, ImportData}
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
        def apply(unit: g.CompilationUnit): Unit = AmmonitePlugin(g)(unit, output)
      }
    }
  )
}
object AmmonitePlugin{
  def apply(g: Global)(unit: g.CompilationUnit, output: Seq[ImportData] => Unit) = {

    def decode(t: g.Tree) = {
      val sym = t.symbol
      (sym.isType, sym.decodedName, sym.decodedName, "")
    }

    val stats = unit.body.children.last.asInstanceOf[g.ModuleDef].impl.body
    val symbols = stats.filter(x => !Option(x.symbol).exists(_.isPrivate))
                       .foldLeft(List.empty[(Boolean, String, String, String)]){
      // These are all the ways we want to import names from previous
      // executions into the current one. Most are straightforward, except
      // `import` statements for which we make use of the typechecker to
      // resolve the imported names
      case (ctx, t @ g.Import(expr, selectors)) =>
        def rec(expr: g.Tree): List[g.Name] = {
          expr match {
            case g.Select(lhs, name) => name :: rec(lhs)
            case g.Ident(name) => List(name)
            case g.This(pkg) => List(pkg)
          }
        }
        val prefix = rec(expr).reverseMap(x => Parsers.backtickWrap(x.decoded)).mkString(".")
        val renamings =
          for(t @ g.ImportSelector(name, _, rename, _) <- selectors) yield {
            Option(rename).map(x => name.decoded -> (name.isTypeName, x.decoded))
          }
        val renameMap = renamings.flatten.map(_.swap).toMap
        val info = new g.analyzer.ImportInfo(t, 0)

        val symNames = for {
          sym <- info.allImportedSymbols
          if !sym.isSynthetic
          if !sym.isPrivate
          if sym.isPublic
          if sym.toString != "package class-use"
          if sym.toString != "object package-info"
          if sym.toString != "class package-info"
        } yield {
          (sym.isType, sym.decodedName)
        }

        val syms = for{
          // For some reason `info.allImportedSymbols` does not show imported
          // type aliases when they are imported directly e.g.
          //
          // import scala.reflect.macros.Context
          //
          // As opposed to via import scala.reflect.macros._.
          // Thus we need to combine allImportedSymbols with the renameMap
          (isType, sym) <- symNames.toList ++ renameMap.keys
        } yield {
          (isType, renameMap.getOrElse((isType, sym), sym), sym, prefix)
        }
        syms ::: ctx
      case (ctx, t @ g.DefDef(_, _, _, _, _, _))  => decode(t) :: ctx
      case (ctx, t @ g.ValDef(_, _, _, _))        => decode(t) :: ctx
      case (ctx, t @ g.ClassDef(_, _, _, _))      => decode(t) :: ctx
      case (ctx, t @ g.ModuleDef(_, _, _))        => decode(t) :: ctx
      case (ctx, t @ g.TypeDef(_, _, _, _))       => decode(t) :: ctx
      case (ctx, t) => ctx
    }

    val grouped =
      symbols.distinct
             .groupBy{case (a, b, c, d) => (b, c, d) }
             .mapValues(_.map(_._1))

//    pprint.log(grouped, "grouped")
    val open = for {
      ((fromName, toName, importString), items) <- grouped
      //              _ = println(fromName + "\t"+ toName)

      // Probably synthetic
      if !fromName.contains("$")
      if fromName != "<init>"
      if fromName != "<clinit>"
      if fromName != "$main"
      // Don't care about this
      if fromName != "toString"
      // Behaves weird in 2.10.x, better to just ignore.
      if fromName != "_"
    } yield {
      val importType = items match{
        case Seq(true) => ImportData.Type
        case Seq(false) => ImportData.Term
        case Seq(_, _) => ImportData.TermType
      }
      ImportData(fromName, toName, importString, importType)
    }
//    pprint.log(open, "open")
    output(open.toVector)
  }
}