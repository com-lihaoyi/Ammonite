package ammonite.repl
import scala.reflect.runtime.universe._
/**
 * Created by haoyi on 2/16/15.
 */
object Main {

  def typeOf[T: WeakTypeTag](t: => T): Type = scala.reflect.runtime.universe.weakTypeOf[T]
  class M
  def main(args: Array[String]): Unit = {

    def typeOf[T: WeakTypeTag]: Type = scala.reflect.runtime.universe.weakTypeOf[T]
    type X = scala.Int with scala.Predef.String{}
    val x = ""
    def rec(tpe: Type) = rec0(tpe, end=true)
    println(rec(typeOf[X]))
    println(rec(typeOf[String]))
    println(rec(typeOf[Int => String]))
    println(rec(typeOf[x.type]))
    println(rec(typeOf[Int]))
    println(rec(typeOf[Main.this.M]))
    println(rec(typeOf[java.util.Set[_ <: String]]))
    println(rec(typeOf[java.util.Set[String]]))
    println(rec(typeOf[Int{val x: Int}]))
    println(rec(typeOf[Int with String]))
    def t[T: TypeTag] = println(rec(typeOf[T]))
    t
    println(rec(typeOf[Main.type]))
    class C{ type V; class U}
    println(rec(typeOf[C#V]))
    println(rec(typeOf[C#U]))
    class T{
      println(rec(typeOf[T.this.type]))
    }
    new T()
    class D
    println(rec(typeOf[D @deprecated]))
    object O{
      class P
    }
    println(rec(typeOf[O.P]))
  }
  def printSym(s: Symbol) = s.name
  def rec0(tpe: Type, end: Boolean = false): String = tpe match {

    case ThisType(sym) => {
      printSym(sym) + (if(sym.isPackage || sym.isModuleClass) "" else ".this.type")
    }
    case SingleType(NoPrefix, sym) => printSym(sym) + (if (end) ".type" else "")
    case SingleType(pre, sym) => rec0(pre) + "." + printSym(sym) + (if (end) ".type" else "")
    case TypeRef(NoPrefix, sym, args) =>
      printSym(sym) + (
        if (args == Nil) "" else "[" + args.map(rec0(_)).mkString(", ") + "]"
      )
    case TypeRef(pre, sym, args) =>
      rec0(pre) + "." + printSym(sym) + (
        if (args == Nil) "" else "[" + args.map(rec0(_)).mkString(", ") + "]"
      )
    case RefinedType(parents, defs) =>
      parents.map(rec0(_)).mkString(" with ") + (
        if (defs == Nil) "" else "{" + defs.mkString(";") + "}"
      )
    case ExistentialType(tparams, result) => rec0(result)
    case AnnotatedType(annots, tp) => rec0(tp) + annots.map(x => " @" + rec0(x.tpe)).mkString
  }
}
