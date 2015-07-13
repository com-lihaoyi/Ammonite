package ammonite.repl.frontend
import language.experimental.macros
import reflect.macros.blackbox.Context
import scala.reflect.macros.TypecheckException

trait TPrintLowPri{
  implicit def default[T]: TPrint[T] = macro TPrintLowPri.typePrintImpl[T]
}
object TPrintLowPri{
  def typePrintImpl[T: c.WeakTypeTag](c: Context): c.Expr[TPrint[T]] = {
    // Used to provide "empty string" values in quasiquotes
    import c.universe._
    val s = ""
    val tpe = weakTypeOf[T]
    def printSymString(s: Symbol) =
      if (s.name.decodedName.toString.startsWith("_$")) "_"
      else s.name.decodedName.toString.stripSuffix(".type")

    def literalColor(s: Tree) = {
      q"$cfgSym.colors.literalColor + ($s) + $cfgSym.colors.endColor"
    }
    def printSym(s: Symbol): Tree = {
      literalColor(q"${printSymString(s)}")
    }

    def printSymFull(s: Symbol): Tree = {
      if (lookup(s)) printSym(s)
      else q"""${printSymFull(s.owner)} + "." + ${printSym(s)}"""

    }
    /**
     * Looks up a symbol in the enclosing scope and returns
     * whether it exists in scope by the same name
     */
    def lookup(s: Symbol) = {
      val cas = c.asInstanceOf[reflect.macros.runtime.Context]
      val g = cas.global
      val gName = s.name.asInstanceOf[g.Name]
      val lookedUps = for(n <- Stream(gName.toTermName, gName.toTypeName)) yield {
        cas.callsiteTyper
          .context
          .lookupSymbol(n, _ => true)
          .symbol
      }

      if (!s.isType) lookedUps.contains(s)
      else {
        // Try to resolve aliases for types
        lookedUps.exists(x => x == s || x.tpe.typeSymbol == s.asInstanceOf[g.Symbol].tpe.typeSymbol)
      }
    }

    def prefixFor(pre: Type, sym: Symbol): Tree = {
      // Depending on what the prefix is, you may use `#`, `.`
      // or even need to wrap the prefix in parentheses
      val sep = pre match{
        case x if x.toString.endsWith(".type") => q""" ${rec0(pre)} + "." """
        case x: TypeRef => q""" ${literalColor(implicitRec(pre))} + "#" """
        case x: SingleType => q""" ${literalColor(rec0(pre))} + "." """
        case x: ThisType => q""" ${literalColor(rec0(pre))} + "." """
        case x => q""" "(" + ${implicitRec(pre)} + ")#" """
      }

      val prefix = if (!lookup(sym)) sep else q"$s"
      q"$prefix + ${printSym(sym)}"
    }


    def printArgSyms(args: List[Symbol]): Tree = {
      def added = args.map{x =>
        val TypeBounds(lo, hi) = x.info
        q""" ${printSym(x)} +  ${printBounds(lo, hi)}"""
      }.reduceLeft[Tree]((l, r) => q"""$l + ", " + $r""")
      if (args == Nil) q"$s" else q""" "[" + $added + "]" """
    }
    def printArgs(args: List[Type]): Tree = {
      def added = args.map(implicitRec(_))
        .reduceLeft[Tree]((l, r) => q"""$l + ", " + $r""")

      if (args == Nil) q"$s" else q""" "[" + $added + "]" """
    }


    def implicitRec(tpe: Type) = {
      try {
        // Make sure the type isn't higher-kinded or some other weird
        // thing, and actually can fit inside the square brackets
        c.typecheck(q"null.asInstanceOf[$tpe]")
        q""" ammonite.repl.frontend.TPrint.implicitly[$tpe].render($cfgSym) """
      }catch{case e: TypecheckException =>
        rec0(tpe)
      }
    }
    def printBounds(lo: Type, hi: Type) = {
      val loTree = if (lo =:= typeOf[Nothing]) q"$s" else q""" " >: " + ${implicitRec(lo)} """
      val hiTree = if (hi =:= typeOf[Any]) q"$s" else q""" " <: " + ${implicitRec(hi)} """
      q"$loTree + $hiTree"
    }

    def showRefinement(quantified: List[Symbol]) = {
      def stmts = for{
        t <- quantified
        suffix <- t.info match {
          case PolyType(typeParams, resultType) =>
            val paramTree = printArgSyms(t.asInstanceOf[TypeSymbol].typeParams)
            val resultBounds = if (resultType =:= typeOf[Any]) q"$s" else q""" " <: " + ${implicitRec(resultType)} """
            Some(q""" $paramTree + $resultBounds""")
          case TypeBounds(lo, hi) if t.toString.contains("$") && lo =:= typeOf[Nothing] && hi =:= typeOf[Any] =>
            None
          case TypeBounds(lo, hi) =>
            Some( printBounds(lo, hi) )
        }
      } yield {
          if (t.toString.endsWith(".type")) {
            val TypeBounds(lo, hi) = t.info
            val RefinedType(parents, defs) = hi
            val filtered = internal.refinedType(parents.filter(x => !(x =:= typeOf[scala.Singleton])), defs)
            q""" "val " + ${literalColor(q"${t.name.toString.stripSuffix(".type")}")} + ": " + ${implicitRec(filtered)}"""
          }else {
            q""" "type " + ${printSym(t)} + $suffix """
          }
        }
      if (stmts.length == 0) None
      else Some(stmts.reduceLeft((l, r) => q""" $l + "; " + $r """))
    }
    /**
     * Decide how to pretty-print, based on the type.
     *
     * This is recursive, but we only rarely use direct recursion: more
     * often, we'll use `implicitRec`, which goes through the normal
     * implicit search channel and can thus
     */
    def rec0(tpe: Type, end: Boolean = false): Tree = tpe match {
      case TypeBounds(lo, hi) =>
        val res = printBounds(lo, hi)
        q""" "_" + $res """
      case ThisType(sym) =>
        q"${printSymFull(sym)} + ${if(sym.isPackage || sym.isModuleClass) "" else ".this.type"}"

      case SingleType(NoPrefix, sym)    => q"${printSym(sym)} + ${if (end) ".type" else ""}"
      case SingleType(pre, sym)         => q"${prefixFor(pre, sym)} + ${if (end) ".type" else ""}"
      // Special-case operator two-parameter types as infix
      case TypeRef(pre, sym, List(left, right))
        if lookup(sym) && sym.name.encodedName.toString != sym.name.decodedName.toString =>

        q"""${implicitRec(left)} + " " + ${printSym(sym)} + " " +${implicitRec(right)}"""

      case TypeRef(NoPrefix, sym, args) => q"${printSym(sym)} + ${printArgs(args)}"
      case TypeRef(pre, sym, args)      => q"${prefixFor(pre, sym)} + ${printArgs(args)}"
      case et @ ExistentialType(quantified, underlying) =>
        showRefinement(quantified) match{
          case None => implicitRec(underlying)
          case Some(block) => q"""${implicitRec(underlying)} + " forSome { " + $block +  " }" """
        }
      case AnnotatedType(annots, tp)    =>
        q"${implicitRec(tp)} + ${annots.map(x => q""" " @" + ${implicitRec(x.tpe)}""").reduceLeft((x, y) => q"$x + $y")}"
      case RefinedType(parents, defs) =>
        val pre =
          if (parents.forall(_ =:= typeOf[AnyRef])) q""" "" """
          else parents.map(implicitRec(_)).reduceLeft[Tree]((l, r) => q"""$l  + " with " + $r""")
        q"$pre + ${
          if (defs.isEmpty) "" else "{" + defs.mkString(";") + "}"
        }"
    }
    lazy val cfgSym = c.freshName[TermName]("cfg")
    val res = c.Expr[TPrint[T]](q"""ammonite.repl.frontend.TPrint.lambda{
      ($cfgSym: pprint.Config) =>
        ${rec0(tpe, end = true)}
    }""")
    //    println("RES " + res)
    res
  }

}
