package ammonite.kernel

import org.scalatest.FreeSpec
import KernelTests._

class EvaluatorTests extends FreeSpec {

  val kernel = buildKernel()

  "simpleExpressions" in {
    checkSuccess(kernel, Vector(
      ("val x = 1 + 2", checkUnit),
      (s"x", checkInt(3)),
      (s"x + x", checkInt(6))
      ))
  }

  "vals" in {
    checkSuccess(kernel, Vector(
      ("val x = 10L", checkUnit),
      ("x", checkLong(10L)),
      ("val y = x + 1", checkUnit),
      ("x * y", checkLong(110L)),
      ("""val `class` = "class" """, checkUnit),
      ("""`class`""", checkString("class"))
      ))
  }

  "lazyvals" in {
    checkSuccess(kernel, Vector(
      ("""lazy val x = 'h'""", checkUnit),
      ("x", checkChar('h')),
      ("""var w = 'l'""", checkUnit),
      ("""lazy val y = {w = 'a'; 'A'}""", checkUnit),
      ("""lazy val z = {w = 'b'; 'B'}""", checkUnit),
      ("z", checkChar('B')),
      ("w", checkChar('b')),
      ("y", checkChar('A')),
      ("w", checkChar('a'))
      ))
  }

  "vars" in {
    checkSuccess(kernel, Vector(
      ("var x: Int = 10", checkUnit),
      ("x", {
        case x : Int => x == 10
        case _ => false
        }),
      ("x = 1", checkUnit),
      ("x", {
        case x: Int => x == 1
        case _ => false
        })
      ))
  }

  "defs" in {
    checkSuccess(kernel, Vector(
      ("def sumItAll[T: Numeric](i: Seq[T]): T = i.sum", checkUnit),
      ("sumItAll(Seq(1, 2, 3, 4, 5))", {
        case i: Int => i == 15
        case _ => false
        }),
      ("sumItAll(Seq(1L, 2L, 3L, 4L, 5L))", {
        case l : Long => l == 15L
        case _ => false
        })
      ))
  }

  "types" in {
    type Funky = Array[Array[String]]
    checkSuccess(kernel, Vector(
      ("type Funky = Array[Array[String]]", checkUnit),
      ("""val arr: Funky = Array(Array("Hello"))""", checkUnit),
      ("arr", {
        case arr: Funky => arr(0).sameElements(Array("Hello"))
        case _ => false
        }),
      ("type Funky2[T] = Array[Array[T]]", checkUnit),
      ("val arr2: Funky2[Int] = Array(Array(1))", checkUnit),
      ("arr2", {
        case arr: Array[Array[Int]] => arr(0).sameElements(Array(1))
        case _ => false
        })
      ))
  }

  "library" in {
    checkSuccess(kernel, Vector(
      ("val x = Iterator.continually(1)", checkUnit),
      ("val y = x take 15", {
        case _ : Unit => true
        case _ => false 
        }),
      ("val z = y.foldLeft(0)(_ + _)", checkUnit),
      ("z", {
        case z: Int => z == 15
        case _ => false
        })
      ))
  }

  "classes" in {
    checkSuccess(kernel, Vector(
      ("""class C{override def toString = "Ceee"}""", checkUnit),
      ("(new C).toString", {
        case s: String => s == "Ceee"
        case _ => false
        }),
      ("case object CO", checkUnit),
      ("CO", checkUnit andThen (!_)),
      ("case class CC()", checkUnit),
      ("CC()", checkUnit andThen (!_)),
      ("case class CO()", checkUnit),
      ("CO", checkUnit andThen (!_)),
      ("CO()", checkUnit andThen (!_))
      ))
  }

  "packageImport" in {
    checkSuccess(kernel, Vector(
      ("import collection.mutable._", checkUnit),
      ("import util.Random", checkUnit),
      ("Random.nextInt()", {
        case _ : Int => true
        case _ => false
        })
      ))
  }

  "nesting" in {
    checkSuccess(kernel, Vector(
      ("val x = 1", checkUnit),
      ("val x = 2", checkUnit),
      ("x", checkInt(2)),
      ("object x {val Y = 1}", checkUnit),
      ("object X {val Y = 2}", checkUnit),
      ("X.Y", checkInt(2))
      ))
  }

  "multistatement" in {
    checkSuccess(kernel, Vector(
      (""";1; 2L; '3'""", checkChar('3')),
      ("val x = 1; x", checkInt(1)),
      ("var x = 1; x = 2; x", checkInt(2)),
      ("var y = 1; case class C(i: Int = 0){ def foo = x + y }; new C().foo", checkInt(3)),
      ("C()", checkUnit andThen (!_))
      ))
  }

  "multiassign" in {
    checkSuccess(kernel, Vector(
      ("val (a, b) = (1, 2)", checkUnit),
      ("a", checkInt(1)),
      ("b", checkInt(2)),
      ("val (a, b) = (11, 12); val (c, d) = (3, 4)", checkUnit),
      ("a", checkInt(11)),
      ("b", checkInt(12)),
      ("c", checkInt(3)),
      ("d", checkInt(4))
      ))
  }

  "parsingProblems" in {
    checkSuccess(kernel, Vector(
      ("(1 + 1)", checkInt(2)),
      ("""(
        1
        +
        1
        )
        """, checkInt(2)),
      ("""(
        (123.0).round
               .toChar
        )""", checkChar('{')),
      ("Seq(0) map {_ + 1}", {
        case x : Seq[_] => x == List(1)
        case _ => false
        })
      ))
  }

  "backticks" in {
    checkSuccess(kernel, Vector(
      ("val `1+1` = 1", checkUnit),
      ("val ++ = 1", checkUnit),
      ("object `+1+`", checkUnit),
      ("val ` ` = 1; type ` ` = Int", checkUnit),
      ("((` ` + `1+1`) : ` `) : Int", checkInt(2)),
      ("object ` `{val ` ` = 333}", checkUnit),
      ("import ` `.` `", checkUnit),
      ("` `", checkInt(333)),
      ("object ` `{val ` ` = 123}", checkUnit),
      ("import ` `.{` ` => `l o l`}", checkUnit),
      ("`l o l`", checkInt(123))
      ))
  }

}
