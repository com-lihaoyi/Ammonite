package ammonite.pprint


import language.experimental.macros
import reflect.macros.blackbox.Context

case class TPrint[T](value: String)
object TPrint{
  def typePrintImpl[T: c.WeakTypeTag](c: Context): c.Expr[TPrint[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    println("typePrintImpl " + tpe)
    val g = c.asInstanceOf[reflect.macros.runtime.Context].global
    val symbols = for{
      ctx <- c.asInstanceOf[reflect.macros.runtime.Context]
        .callsiteTyper
        .context
        .enclosingContextChain

      sym <- ctx.imports.flatMap(_.allImportedSymbols) ++ ctx.scope.toList
    } yield sym
    val symbolSet: Set[c.universe.Symbol] = symbols.map(_.asInstanceOf[c.universe.Symbol]).toSet
    //    val fullNames = symbols.map(_.fullName).filter(!_.contains("$"))
    def printSym(s: Symbol) = s.name
    def prefixFor(pre: Type, sym: Symbol) = {
//      println(pre.getClass)
      val sep = if (pre.isInstanceOf[TypeRef]) "#" else "."
      val prefix = if (!symbolSet.contains(sym)) rec0(pre) + sep else ""
      prefix + printSym(sym)
    }
    def printArgs(args: List[Type]) = {
      if (args == Nil) "" else "[" + args.map(rec0(_)).mkString(", ") + "]"
    }

    def rec0(tpe: Type, end: Boolean = false): String = tpe match {
      case ThisType(sym) =>
        printSym(sym) + (if(sym.isPackage || sym.isModuleClass) "" else ".this.type")

      case SingleType(NoPrefix, sym)    => printSym(sym) + (if (end) ".type" else "")
      case SingleType(pre, sym)         => prefixFor(pre, sym) + (if (end) ".type" else "")
      case TypeRef(NoPrefix, sym, args) => printSym(sym) + printArgs(args)
      case TypeRef(pre, sym, args)      => prefixFor(pre, sym) + printArgs(args)
      case ExistentialType(tparams, result) =>
//        println("EXISTENTIAL " +
//          tparams.map(_.asInstanceOf[g.AbstractTypeSymbol].existentialBound)
//          + "\t" + result)
        rec0(result)
      case AnnotatedType(annots, tp)    => rec0(tp) + annots.map(x => " @" + rec0(x.tpe)).mkString
      case RefinedType(parents, defs) =>
        parents.map(rec0(_)).mkString(" with ") + (
          if (defs.isEmpty) "" else "{" + defs.mkString(";") + "}"
          )
    }

    c.Expr[TPrint[T]](q"new ammonite.pprint.TPrint(${rec0(tpe, end = true)})")
  }
  implicit def default[T]: TPrint[T] = macro typePrintImpl[T]


}