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
      override val runsBefore = List("patmat")
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
    val ignoredSyms = Set(
      "package class-use",
      "object package-info",
      "class package-info"
    )
    val ignoredNames = Set(
      // Probably synthetic
      "<init>",
      "<clinit>",
      "$main",
      // Don't care about this
      "toString",
      // Behaves weird in 2.10.x, better to just ignore.
      "_"
    )
    def saneSym(sym: g.Symbol): Boolean = {
      !sym.name.decoded.contains('$') &&
      sym.exists &&
      !sym.isSynthetic &&
      !sym.isPrivate &&
      !sym.isProtected &&
      sym.isPublic &&
      !ignoredSyms(sym.toString) &&
      !ignoredNames(sym.name.decoded)
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


        /**
          * A map of each name importable from `expr`, to a `Seq[Boolean]`
          * containing a `true` if there's a type-symbol you can import, `false`
          * if there's a non-type symbol and both if there are both type and
          * non-type symbols that are importable for that name
          */
        val importableIsTypes =
          expr.tpe
              .members
              .filter(saneSym(_))
              .groupBy(_.name.decoded)
              .mapValues(_.map(_.isType).toVector)


        val renamings = for{
          t @ g.ImportSelector(name, _, rename, _) <- selectors
          isType <- importableIsTypes.getOrElse(name.decode, Nil) // getOrElse just in case...
        } yield Option(rename).map(x => name.decoded ->  (isType, x.decoded))

        val renameMap = renamings.flatten.map(_.swap).toMap
        val info = new g.analyzer.ImportInfo(t, 0)

        val symNames = for {
          sym <- info.allImportedSymbols
          if saneSym(sym)
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
          (isType, sym) <- (symNames.toList ++ renameMap.keys).distinct
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


    val open = for {
      ((fromName, toName, importString), items) <- grouped
      if !ignoredNames(fromName)
    } yield {
      val importType = items match{
        case Seq(true) => ImportData.Type
        case Seq(false) => ImportData.Term
        case Seq(_, _) => ImportData.TermType
      }
      ImportData(fromName, toName, importString, importType)
    }

    output(open.toVector)
  }
}