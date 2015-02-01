package ammonite.sh

import ammonite.pprint.Config.Defaults._
import ammonite.pprint.PPrint
import utest._


import scala.collection.{immutable => imm}
import scala.reflect.io.VirtualDirectory

object EvaluatorTests extends TestSuite{

  val tests = TestSuite{
    val check = new Checker()
    'simpleExpressions{
      check("1 + 2", "res0: Int = 3")
      check("res0", "res1: Int = 3")
      check("res0 + res1", "res2: Int = 6")
    }
    'vals{
      check("val x = 10L", "x: Long = 10L")
      check("x", "res1: Long = 10L")
      check("val y = x + 1", "y: Long = 11L")
      check("x * y", "res3: Long = 110L")
    }
    'lazyvals{
      // It actually appears when I ask for it
      check("lazy val x = 'h'", "x: Char = <lazy>")
      check("x", "res1: Char = 'h'")

      // The actual evaluation happens in the correct order
      check("var w = 'l'", "w: Char = 'l'")
      check("lazy val y = {w = 'a'; 'A'}", "y: Char = <lazy>")
      check("lazy val z = {w = 'b'; 'B'}", "z: Char = <lazy>")
      check("z", "res5: Char = 'B'")
      check("y", "res6: Char = 'A'")
      check("w", "res7: Char = 'a'")
    }

    'vars{
      check("var x: Int = 10", "x: Int = 10")
      check("x", "res1: Int = 10")
      check("x = 1", "res2: Unit = ()")
      check("x", "res3: Int = 1")
    }

    'defs{
      check("def sumItAll[T: Numeric](i: Seq[T]): T = {i.sum}", "defined function sumItAll")
      check("sumItAll(Seq(1, 2, 3, 4, 5))", "res1: Int = 15")
      check("sumItAll(Seq(1L, 2L, 3L, 4L, 5L))", "res2: Long = 15L")
    }
    'types{
      check("type Funky = Array[Array[String]]", "defined type Funky")
      check(
        """val arr: Funky = Array(Array("Hello!"))""",
        """arr: $res0.Funky = Array(Array("Hello!"))"""
      )
      check("type Funky2[T] = Array[Array[T]]", "defined type Funky2")
      check(
        """val arr: Funky2[Int] = Array(Array(123))""",
        """arr: $res2.Funky2[Int] = Array(Array(123))"""
      )
    }
    'library{
      check("val x = Iterator.continually(1)", "x: Iterator[Int] = non-empty iterator")
      check("val y = x.take(15)", "y: Iterator[Int] = non-empty iterator")
      check("val z = y.foldLeft(0)(_ + _)", "z: Int = 15")
    }

    'import0 {
      check("import math.abs", "import math.abs")
      check("val abs = 123L", "abs: Long = 123L")
      check("abs", "res2: Long = 123L")
    }

    'import{
      check("val abs = 'a'", "abs: Char = 'a'")
      check("abs", "res1: Char = 'a'")
      check("val abs = 123L", "abs: Long = 123L")
      check("abs", "res3: Long = 123L")
      check("import math.abs", "import math.abs")
      check("abs(-10)", "res5: Int = 10")
      check("val abs = 123L", "abs: Long = 123L")
      check("abs", "res7: Long = 123L")
      check("import java.lang.Math._", "import java.lang.Math._")
      check("abs(-4)", "res9: Int = 4")
    }

    'packageImport{
      check("import ammonite.pprint._")
      check("import Config.Defaults._")
    }

    'nesting{
      check("val x = 1", "x: Int = 1")
      check("val x = 2", "x: Int = 2")
      check("x", "res2: Int = 2")
      check("object X{ val Y = 1 }", "defined object X")
      check("object X{ val Y = 2 }", "defined object X")
      check("X.Y", "res5: Int = 2")
    }
  }
}
