package ammonite.pprint


import language.experimental.macros
import reflect.macros.blackbox.Context

/**
 * Summoning an implicit `TPrint[T]` provides a pretty-printed
 * string representation of the type `T`, much better than is
 * provided by the default `Type#toString`. In particular
 *
 * - More forms are properly supported and printed
 * - Prefixed Types are printed un-qualified, according to
 *   what's currently in scope
 */
case class TPrint[T](value: String)

object TPrint extends TPrintGen[TPrint] with TPrintLowPri{
  def make[T](s: String) = TPrint[T](s)
  def get[T](implicit t: TPrint[T]) = t.value
  def implicitly[T](implicit t: TPrint[T]): TPrint[T] = t
}
trait TPrintLowPri{
  implicit def default[T]: TPrint[T] = macro TPrintLowPri.typePrintImpl[T]
}
object TPrintLowPri{
  def typePrintImpl[T: c.WeakTypeTag](c: Context): c.Expr[TPrint[T]] = {
    import c.universe._
    // Used to provide "empty string" values in quasiquotes
    val s = ""
    val tpe = weakTypeOf[T]

    def printSym(s: Symbol): String = {
      if (s.name.toString.startsWith("_$")) "_"
      else s.name.toString.stripSuffix(".type")
    }
    def printSymFull(s: Symbol): String = {
      val curr =
        if (s.name.toString.startsWith("_$")) "_"
        else s.name.toString.stripSuffix(".type")
      if (lookup(s)) curr
      else printSymFull(s.owner) + "." + curr

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


      val res = {
        if (!s.isType) lookedUps.contains(s)
        else {
          // Try to resolve aliases for types
          lookedUps.exists(x => x == s || x.tpe.typeSymbol == s.asInstanceOf[g.Symbol].tpe.typeSymbol)
        }
      }

      res
    }

    def prefixFor(pre: Type, sym: Symbol): Tree = {
      // Depending on what the prefix is, you may use `#`, `.`
      // or even need to wrap the prefix in parentheses
      val sep = pre match{
        case x if x.toString.endsWith(".type") => q""" ${rec0(pre)} + "." """
        case x: TypeRef => q""" ${implicitRec(pre)} + "#" """
        case x: SingleType => q""" ${rec0(pre)} + "." """
        case x: ThisType => q""" ${rec0(pre)} + "." """
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


    def implicitRec(tpe: Type) = q""" ammonite.pprint.TPrint.implicitly[$tpe].value """
    def printBounds(lo: Type, hi: Type) = {
      val loTree = if (lo =:= typeOf[Nothing]) q"$s" else q""" " >: " + ${implicitRec(lo)} """
      val hiTree = if (hi =:= typeOf[Any]) q"$s" else q""" " <: " + ${implicitRec(hi)} """
      q"$loTree + $hiTree"
    }
    def rec0(tpe: Type, end: Boolean = false) = tpe match {
      case TypeBounds(lo, hi) =>
        val res = printBounds(lo, hi)
        q""" "_" + $res """
      case ThisType(sym) =>
        q"${printSymFull(sym)} + ${if(sym.isPackage || sym.isModuleClass) "" else ".this.type"}"

      case SingleType(NoPrefix, sym)    => q"${printSym(sym)} + ${if (end) ".type" else ""}"
      case SingleType(pre, sym)         => q"${prefixFor(pre, sym)} + ${if (end) ".type" else ""}"
      case TypeRef(NoPrefix, sym, args) => q"${printSym(sym)} + ${printArgs(args)}"
      case TypeRef(pre, sym, args)      => q"${prefixFor(pre, sym)} + ${printArgs(args)}"
      case et @ ExistentialType(quantified, underlying) =>
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
              q""" "val " + ${t.name.toString.stripSuffix(".type")} + ": " + ${implicitRec(filtered)}"""
            }else {
              q""" "type " + ${printSym(t)} + $suffix """
            }
          }
        def stmtBlock = stmts.reduceLeft((l, r) => q""" $l + "; " + $r """)
        if (stmts.length == 0) implicitRec(underlying)
        else q"""${implicitRec(underlying)} + " forSome { " + $stmtBlock +  " }" """
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

    c.Expr[TPrint[T]](q"new ammonite.pprint.TPrint(${rec0(tpe, end = true)})")
  }

}
