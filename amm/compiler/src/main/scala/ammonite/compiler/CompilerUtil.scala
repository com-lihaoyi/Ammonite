package ammonite.compiler

object CompilerUtil {

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
    "equals",
    "wait",
    "notify",
    "notifyAll",
    "synchronized",
    "hashCode",
    "getClass",
    "eq",
    "ne",
    "##",
    "==",
    "!=",
    "isInstanceOf",
    "asInstanceOf",
    // Behaves weird in 2.10.x, better to just ignore.
    "_"
  )

}
