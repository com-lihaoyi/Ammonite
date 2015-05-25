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
trait TPrint[T]{
  def render(implicit cfg: Config): String
}

object TPrint extends TPrintGen[TPrint, Config] with TPrintLowPri{
  def literal[T](s: String) = new TPrint[T]{
    def render(implicit cfg: Config) = cfg.color.literal(s)
  }
  def lambda[T](f: Config => String) = new TPrint[T]{
    def render(implicit cfg: Config) = f(cfg)
  }
  def make[T](f: Config => String) = TPrint.lambda[T](f)
  def get[T](cfg: Config)(implicit t: TPrint[T]) = t.render(cfg)
  def implicitly[T](implicit t: TPrint[T]): TPrint[T] = t
  implicit val NothingTPrint: TPrint[Nothing] = TPrint.literal("Nothing")
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
    def printSymString(s: Symbol) =
      if (s.name.decodedName.toString.startsWith("_$")) "_"
      else s.name.decodedName.toString.stripSuffix(".type")

    def printSym(s: Symbol): Tree = {
      q"""$cfgSym.color.literal(${printSymString(s)})"""
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
        case x: TypeRef => q""" $cfgSym.color.literal(${implicitRec(pre)}) + "#" """
        case x: SingleType => q""" $cfgSym.color.literal(${rec0(pre)}) + "." """
        case x: ThisType => q""" $cfgSym.color.literal(${rec0(pre)}) + "." """
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


    def implicitRec(tpe: Type) = q""" ammonite.pprint.TPrint.implicitly[$tpe].render($cfgSym) """
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
            q""" "val " + $cfgSym.color.literal(${t.name.toString.stripSuffix(".type")}) + ": " + ${implicitRec(filtered)}"""
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
    def rec0(tpe: Type, end: Boolean = false) = tpe match {
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
    val res = c.Expr[TPrint[T]](q"""ammonite.pprint.TPrint.lambda{
      ($cfgSym: ammonite.pprint.Config) =>
        ${rec0(tpe, end = true)}
    }""")
//    println("RES " + res)
    res
  }

}
