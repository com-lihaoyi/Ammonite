package ammonite.pprint


import language.experimental.macros
import reflect.macros.blackbox.Context

case class TPrint[T](value: String)
object TPrint{
  val s = ""
  def typePrintImpl[T: c.WeakTypeTag](c: Context): c.Expr[TPrint[T]] = {
    import c.universe._

    val tpe = weakTypeOf[T]
    println("TYPE \t" +  tpe)
    def printSym(s: Symbol) = {
      if (s.name.toString.startsWith("_$")) "_"
      else s.name.toString.stripSuffix(".type")
    }

    def lookup(s: Symbol) = {
      val cas = c.asInstanceOf[reflect.macros.runtime.Context]
      val g = cas.global
      val lookedUp = cas.callsiteTyper
        .context
        .lookupSymbol(s.name.asInstanceOf[g.Name], _ => true)
        .symbol
      if (!s.isType) lookedUp == s
      else{
        // Try to resolve aliases for types
        lookedUp == s || lookedUp.tpe.typeSymbol == s.asInstanceOf[g.Symbol].tpe.typeSymbol
      }
    }

    def prefixFor(pre: Type, sym: Symbol): Tree = {
      println("prefixFor " + pre + "\t" + pre.getClass + "\t" + sym )
      val sep = pre match{
        case x if x.toString.endsWith(".type") => q""" ${rec0(pre)} + "." """
        case _: TypeRef => q""" ${implicitRec(pre)} + "#" """
        case _: SingleType => q""" ${rec0(pre)} + "." """
        case _: ThisType => q""" ${rec0(pre)} + "." """
        case x => q""" "(" + ${implicitRec(pre)} + ")#" """
      }

      val s = ""
      val prefix = if (!lookup(sym)) sep else q"$s"
      q"$prefix + ${printSym(sym)}"
    }

    def printArgs(args: List[Type]): Tree = {
      val s = ""
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
//        println(s"TypeBounds $lo $hi $res")
        q""" "_" + $res """
      case ThisType(sym) =>
        q"${printSym(sym)} + ${if(sym.isPackage || sym.isModuleClass) "" else ".this.type"}"

      case SingleType(NoPrefix, sym)    => q"${printSym(sym)} + ${if (end) ".type" else ""}"
      case SingleType(pre, sym)         => q"${prefixFor(pre, sym)} + ${if (end) ".type" else ""}"
      case TypeRef(NoPrefix, sym, args) => q"${printSym(sym)} + ${printArgs(args)}"
      case TypeRef(pre, sym, args)      => q"${prefixFor(pre, sym)} + ${printArgs(args)}"
      case ExistentialType(quantified, underlying) =>
//        println("EX " + et)
        def stmts = for{
          t <- quantified
          _ = println("T " + t)
          suffix <- t.info match {
            case PolyType(typeParams, resultType) =>
//              println("PolyType " + t.info)
              val paramTree = printArgs(t.asInstanceOf[TypeSymbol].typeParams.map(_.info))
              typeParams.map(_.info).map{case TypeBounds(lo, hi) => printBounds(lo, hi)}
              val resultBounds = if (resultType =:= typeOf[Any]) q"$s" else q""" " <: " + ${implicitRec(resultType)} """
              Some(q""" $paramTree + $resultBounds""")
            case TypeBounds(lo, hi) if t.toString.contains("$") && lo =:= typeOf[Nothing] && hi =:= typeOf[Any] =>
              None
            case TypeBounds(lo, hi) => Some( printBounds(lo, hi) )
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
        q"${pre} + ${
          if (defs.isEmpty) "" else "{" + defs.mkString(";") + "}"
        }"
    }

    c.Expr[TPrint[T]](q"new ammonite.pprint.TPrint(${rec0(tpe, end = true)})")
  }

  implicit def default[T]: TPrint[T] = macro typePrintImpl[T]
  def implicitly[T](implicit t: TPrint[T]): TPrint[T] = t
}
