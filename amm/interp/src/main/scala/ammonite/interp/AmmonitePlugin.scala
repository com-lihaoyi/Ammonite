package ammonite.interp

import ammonite.util.{ImportData, Name, Util}

import scala.reflect.NameTransformer
import scala.tools.nsc._
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.reflect.internal.util._

/**
 * Used to capture the names in scope after every execution, reporting them
 * to the `output` function. Needs to be a compiler plugin so we can hook in
 * immediately after the `typer`
 */
class AmmonitePlugin(g: scala.tools.nsc.Global,
                     output: Seq[ImportData] => Unit,
                     usedEarlierDefinitions: Seq[String] => Unit,
                     userCodeNestingLevel: => Int,
                     topWrapperLen: => Int) extends Plugin{
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
        def apply(unit: g.CompilationUnit): Unit = {
          val things = global.currentRun.units.map(_.source.path).toList
          AmmonitePlugin(g)(
            unit, output, usedEarlierDefinitions, userCodeNestingLevel, topWrapperLen
          )
        }
      }
    },

    new PluginComponent {
      val global = g

      val runsAfter = List("parser")
      override val runsBefore = List("namer")
      val phaseName = "FixLineNumbers"

      def newPhase(prev: Phase): Phase = new g.GlobalPhase(prev) {

        def name = phaseName
        def apply(unit: g.CompilationUnit): Unit = {
          val things = global.currentRun.units.map(_.source.path).toList
          LineNumberModifier(g)(unit, topWrapperLen)
        }
      }
    }
  )
}


object AmmonitePlugin{
  var count = 0
  def apply(g: Global)
           (unit: g.CompilationUnit,
            output: Seq[ImportData] => Unit,
            usedEarlierDefinitions: Seq[String] => Unit,
            userCodeNestingLevel: => Int,
            topWrapperLen: => Int) = {


    count += 1
    def decode(t: g.Tree) = {
      val sym = t.symbol
      (sym.isType, sym.decodedName, sym.decodedName, Seq())
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

    val stats = {
      val nestingLevel = userCodeNestingLevel
      assert(nestingLevel >= 0)
      (0 until nestingLevel).foldLeft(unit.body.children.last.children)((res, _) =>
        res.last.asInstanceOf[g.ImplDef].impl.body
      )
    }

    userCodeNestingLevel match {
      case 1 =>
        /*
         * We don't try to determine what previous commands are actually used here.
         * userCodeNestingLevel == 1 likely corresponds to the default object-based
         * code wrapper, which doesn't rely on the actually used previous commands.
         */

      case 2 =>
        /*
         * For userCodeNestingLevel >= 2, we list the variables from the first wrapper
         * used from the user code.
         *
         * E.g. if, after wrapping, the code looks like
         * ```
         *   class cmd2 {
         *
         *     val cmd0 = ???
         *     val cmd1 = ???
         *
         *     import cmd0.{
         *       n
         *     }
         *
         *     class Helper {
         *       // user-typed code
         *       val n0 = n + 1
         *     }
         *   }
         * ```
         * this would process the tree of `val n0 = n + 1`, find `n` as a tree like
         * `cmd2.this.cmd0.n`, and put `cmd0` in `uses`.
         */
        val wrapperSym = unit.body.children.last.children
          .last.asInstanceOf[g.ImplDef].symbol
        val uses0 = for {
          tree <- stats
          names <- tree.collect {
            case g.Select(node, g.TermName(name)) if node.symbol == wrapperSym =>
              name :: Nil
            case tt @ g.TypeTree() =>
              tt.tpe.collect {
                case g.SingleType(pre, sym) if pre.typeSymbol == wrapperSym =>
                  sym.name.decoded
              }
          }
          name <- names
        } yield name

        usedEarlierDefinitions(uses0.distinct)
    }

    val symbols = stats.filter(x => !Option(x.symbol).exists(_.isPrivate))
                       .foldLeft(List.empty[(Boolean, String, String, Seq[Name])]){
      // These are all the ways we want to import names from previous
      // executions into the current one. Most are straightforward, except
      // `import` statements for which we make use of the typechecker to
      // resolve the imported names
      case (ctx, t @ g.Import(expr, selectors)) =>

        def rec(expr: g.Tree): List[(g.Name, g.Symbol)] = {
          expr match {
            case s @ g.Select(lhs, _) => (s.symbol.name -> s.symbol) :: rec(lhs)
            case i @ g.Ident(name) => List(name -> i.symbol)
            case t @ g.This(pkg) => List(pkg -> t.symbol)
          }
        }
        val (nameList, symbolList) = rec(expr).reverse.unzip

        // Note: we need to take the symbol on the left-most name and get it's
        // `.fullName`. Otherwise if we're in
        //
        // ```
        // package foo.bar.baz
        // object Wrapper{val x = ...; import x._}
        // ```
        //
        // The import will get treated as from `Wrapper.x`, but the person
        // running that import will not be in package `foo.bar.baz` and will
        // not be able to find `Wrapper`! Thus we need to get the full name.
        // In cases where the left-most name is a top-level package,
        // `.fullName` is basically a no-op and it works as intended.
        //
        // Apart from this, all other imports should resolve either to one
        // of these cases or importing-from-an-existing import, both of which
        // should work without modification

        val headFullPath = NameTransformer.decode(symbolList.head.fullName).split('.').map(Name(_))
        // prefix package imports with `_root_` to try and stop random
        // variables from interfering with them. If someone defines a value
        // called `_root_`, this will still break, but that's their problem
        val rootPrefix = if(symbolList.head.isPackage) Seq(Name("_root_")) else Nil
        val tailPath = nameList.tail.map(_.decoded).map(Name(_))

        val prefix = rootPrefix ++ headFullPath ++ tailPath

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
        val info = new g.analyzer.ImportInfo(t, 0, false)

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

      ImportData(Name(fromName), Name(toName), importString, importType)
    }

    // Send the recorded imports through a callback back to the Ammonite REPL.
    // Make sure we sort the imports according to their prefix, so that when
    // they later get rendered the same-prefix imports can be collapsed
    // together v.s. having them by sent in the arbitrary-jumbled order they
    // come out of the `grouped` map in

    output(open.toVector.sortBy(x => Util.encodeScalaSourcePath(x.prefix)))
  }
}


object LineNumberModifier {
  def apply(g: Global)(unit: g.CompilationUnit,
                       topWrapperLen: => Int) = {

    object LineNumberCorrector extends g.Transformer {
      import scala.reflect.internal.util._

      private val trimmedSource = new BatchSourceFile(g.currentSource.file,
        g.currentSource.content.drop(topWrapperLen))

      override def transform(tree: g.Tree) = {
        val transformedTree = super.transform(tree)
        // The `start` and `end` values in transparent/range positions are left
        // untouched, because of some aggressive validation in scalac that checks
        // that trees are not overlapping, and shifting these values here
        // violates the invariant (which breaks Ammonite, potentially because
        // of multi-stage).
        // Moreover, we rely only on the "point" value (for error reporting).
        // The ticket https://github.com/scala/scala-dev/issues/390 tracks down
        // relaxing the aggressive validation.
        val newPos = tree.pos match {
          case s : TransparentPosition if s.start > topWrapperLen =>
              new TransparentPosition(trimmedSource, s.start, s.point - topWrapperLen, s.end)
          case s: RangePosition if s.start > topWrapperLen =>
              new RangePosition(trimmedSource, s.start, s.point - topWrapperLen, s.end)
          case s: OffsetPosition if s.start > topWrapperLen =>
              new OffsetPosition(trimmedSource, s.point - topWrapperLen)
          case s => s

        }
        transformedTree.pos = newPos
        transformedTree
      }

      def apply(unit: g.CompilationUnit) = transform(unit.body)
    }

    unit.body = LineNumberCorrector(unit)
  }
}
