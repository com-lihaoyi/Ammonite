package ammonite.repl

/**
 * Created by haoyi on 2/16/15.
 */
object Main {
  def main(args: Array[String]): Unit = {
    println("""\\s""")
    println(s"""\\s""")

//    val mirror = scala.reflect.runtime.currentMirror
//    val u = mirror.universe
//    val rm = mirror.reflectModule(mirror.RootPackage)
//    def rec(t: u.Symbol, d: Int = 0): Unit = if (t.isPackage && d < 5){
//      println("  " * d + t)
//      t.typeSignature.members.filter(_ != t).foreach(rec(_, d + 1))
//    }
//
//    rec(rm.symbol)
  }
}
