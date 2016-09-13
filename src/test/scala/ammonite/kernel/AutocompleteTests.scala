package ammonite.kernel

import language.postfixOps
import org.scalatest.FreeSpec

class AutocompleteTests extends FreeSpec {

  val kernel = KernelTests.buildKernel()

  def complete(caretCode: String, cmp: (Set[String]) => Set[String], sigs: (Set[String]) => Set[String] = _ => Set()) = {
    val cursor = caretCode.indexOf("<caret>")
    val buf = caretCode.replace("<caret>", "")
    val (_, completions, signatures) = kernel.complete(buf, cursor)
    val left = cmp(completions.toSet)
    assert(left.isEmpty, "cmp failed")
    val sigLeft = sigs(signatures.toSet)
    assert(sigLeft.isEmpty, "sigs failed")
  }

  val anyCompletion = Set(
    "!=",
    "==",
    "toString",
    "equals",
    "hashCode",
    "getClass",
    "asInstanceOf",
    "isInstanceOf"
  )

  implicit class SetExt[T](s1: Set[T]) {
    def ^(s2: Set[T]): Set[T] = (s1 diff s2) | (s2 diff s1)
  }

  "import" in {
    //complete("""import j<caret>""", Set("java", "javax", "javafx", "jdk") -- _)
    //complete("""import ja<caret>""", Set("java", "javax", "javafx") -- _)
    complete("""import java.<caret>""", Set("lang", "util") -- _)
    complete("""import java.u<caret>""", Set("util") ^ _)
    complete("""import java.util.<caret>""", Set("LinkedHashMap", "LinkedHashSet") -- _)
    complete("""import java.util.LinkedHa<caret>""", Set("LinkedHashMap", "LinkedHashSet") ^ _)
    complete(
      """import java.util.{LinkedHa<caret>""",
      Set("LinkedHashMap", "LinkedHashSet") ^ _
    )
    complete(
      """import java.util.{LinkedHashMap, Linke<caret>""",
      Set("LinkedHashMap", "LinkedHashSet", "LinkedList") ^ _
    )
    complete(
      """import scala.uti.<caret>""",
      Set.empty[String] -- _
    )
    complete(
      """import scala.colltion.<caret>""",
      Set.empty[String] -- _
    )
  }

  "scope" in {
    complete("""<caret>""", Set("scala") -- _)
    complete("""Seq(1, 2, 3).map(argNameLol => <caret>)""", Set("argNameLol") -- _)
    complete("""object Zomg{ <caret> }""", Set("Zomg") -- _)
    complete(
      "printl<caret>",
      Set("println") ^,
      Set[String]() ^
    )
  }

  "scopePrefix" in {
    complete("""ammon<caret>""", Set("ammonite") ^ _)

    complete("""Seq(1, 2, 3).map(argNameLol => argNam<caret>)""", Set("argNameLol") ^)

    complete("""object Zomg{ Zom<caret> }""", Set("Zomg") ^)
    complete("""object Zomg{ Zo<caret>m }""", Set("Zomg") ^)
    complete("""object Zomg{ Z<caret>om }""", Set("Zomg") ^)
    complete("""object Zomg{ <caret>Zom }""", Set("Zomg") ^)
  }

  "dot" in {

    complete("""java.math.<caret>""", Set("MathContext", "BigDecimal", "BigInteger", "RoundingMode") ^)

    complete("""scala.Option.<caret>""", (anyCompletion ++ Set("apply", "empty")) ^)

    complete("""Seq(1, 2, 3).map(_.<caret>)""", (anyCompletion ++ Set("+", "-", "*", "/")) -- _)

    complete("""val x = 1; x + (x.<caret>)""", Set("-", "+", "*", "/") -- _)

  }

  "deep" in {
    complete("""fromN<caret>""", Set("scala.concurrent.duration.fromNow") ^)
    complete("""Fut<caret>""", Set("scala.concurrent.Future", "java.util.concurrent.Future") -- _)
    complete("""SECO<caret>""", Set("scala.concurrent.duration.SECONDS") ^)
  }

  "dotPrefix" in {
    complete("""java.math.Big<caret>""", Set("BigDecimal", "BigInteger") ^)
    complete("""scala.Option.option2<caret>""", Set() ^)
    complete("""val x = 1; x + x.><caret>""",
             Set(">>", ">>>") -- _,
             Set(
               "def >(x: Double): Boolean",
               "def >(x: Float): Boolean",
               "def >(x: Int): Boolean",
               "def >(x: Short): Boolean",
               "def >(x: Long): Boolean",
               "def >(x: Char): Boolean",
               "def >(x: Byte): Boolean"
             ) ^)
  }

  "defTab" in {
    //Assert no NullPointerException was thrown. Does not verify any completions.
    complete("""def<caret>""", Set.empty -- _)
  }

  "Array" in {
    //Test around https://github.com/lihaoyi/Ammonite/issues/252
    complete("""new Array<caret>""", Set() ^)
  }

}
