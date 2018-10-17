package ammonite.session

import ammonite.DualTestRepl
import ammonite.TestUtils._
import utest._

import scala.collection.{immutable => imm}

object EvaluatorTests extends TestSuite{

  val tests = Tests{
    println("EvaluatorTests")
    val check = new DualTestRepl()
    'simpleExpressions{
      check.session("""
        @ 1 + 2
        res0: Int = 3

        @ res0
        res1: Int = 3

        @ res0 + res1
        res2: Int = 6
      """)
    }
    'vals{
      check.session("""
        @ val x = 10L
        x: Long = 10L

        @ x
        res1: Long = 10L

        @ val y = x + 1
        y: Long = 11L

        @ x * y
        res3: Long = 110L

        @ val `class` = "class"
        `class`: String = "class"

        @ `class`
        res5: String = "class"
      """)
    }
    'lazyvals{
      // It actually appears when I ask for it, and the
      // actual evaluation happens in the correct order
      check.session("""
        @ lazy val x = 'h'
        x: Char = <lazy>

        @ x
        res1: Char = 'h'

        @ var w = 'l'
        w: Char = 'l'

        @ lazy val y = {w = 'a'; 'A'}
        y: Char = <lazy>

        @ lazy val z = {w = 'b'; 'B'}
        z: Char = <lazy>

        @ z
        res5: Char = 'B'

        @ y
        res6: Char = 'A'

        @ w
        res7: Char = 'a'
      """)
    }

    'vars{
      check.session("""
        @ var x: Int = 10
        x: Int = 10

        @ x
        res1: Int = 10

        @ x = 1

        @ x
        res3: Int = 1
      """)
    }

    'defs{
      check.session("""
        @ def sumItAll[T: Numeric](i: Seq[T]): T = {i.sum}
        defined function sumItAll

        @ sumItAll(Seq(1, 2, 3, 4, 5))
        res1: Int = 15

        @ sumItAll(Seq(1L, 2L, 3L, 4L, 5L))
        res2: Long = 15L
      """)
    }
    'types{
      check.session(s"""
        @ type Funky = Array[Array[String]]
        defined type Funky

        @ val arr: Funky = Array(Array("Hello!"))
        arr: Funky = Array(Array("Hello!"))

        @ type Funky2[T] = Array[Array[T]]
        defined type Funky2

        @ val arr: Funky2[Int] = Array(Array(123))
        arr: Funky2[Int] = Array(Array(123))
      """)
    }
    'library{
      // x and y pprinted value is 'non-empty iterator' up to 2.12.6,
      // '<iterator>' since 2.12.7, hence the '?' (don't check the value)
      check.session("""
        @ val x = Iterator.continually(1)
        x: Iterator[Int] = ?

        @ val y = x.take(15)
        y: Iterator[Int] = ?

        @ val z = y.foldLeft(0)(_ + _)
        z: Int = 15
      """)
    }

    'classes{
      check.session(s"""
        @ class C{override def toString() = "Ceee"}
        defined class C

        @ new C
        res1: C = Ceee

        @ case object CO
        defined object CO

        @ CO
        res3: CO.type = CO

        @ case class CC()
        defined class CC

        @ CC()
        res5: CC = CC()

        @ CO
        res6: CO.type = CO

        @ case class CO()
        defined class CO

        @ CO // res8: CO.type = CO

        @ CO()
        res9: CO = CO()

        @ CO == res3
        res10: Boolean = false
      """)

      // there used to be a
      //     @ CO
      //     res8: CO.type = CO
      // that fails with class wrapping in 2.12 since #853, giving instead
      //     res8: CO = CO
      // CO != res3 should test roughly the same thing
    }

    'packageImport{
      check.session("""
        @ import java.util._

        @ import concurrent.atomic._
      """)
    }

    'nesting{
      check.session("""
        @ val x = 1
        x: Int = 1

        @ val x = 2
        x: Int = 2

        @ x
        res2: Int = 2

        @ object X{ val Y = 1 }
        defined object X

        @ object X{ val Y = 2 }
        defined object X

        @ X.Y
        res5: Int = 2
      """)
    }
    'multistatement{
      check.session(s"""
        @ ;1; 2L; '3';
        res0_0: Int = 1
        res0_1: Long = 2L
        res0_2: Char = '3'

        @ val x = 1; x;
        x: Int = 1
        res1_1: Int = 1

        @ var x = 1; x = 2; x
        x: Int = 2
        res2_2: Int = 2

        @ var y = 1; case class C(i: Int = 0){ def foo = x + y }; new C().foo
        y: Int = 1
        defined class C
        res3_2: Int = 3

        @ C()
        res4: C = C(0)
      """)
    }

    'multiassign{
      check.session("""
        @ val (a, b) = (1, 2)
        a: Int = 1
        b: Int = 2

        @ a
        res1: Int = 1

        @ val (a, b) = (1, 2); val (c, d) = (3, 4)
        a: Int = 1
        b: Int = 2
        c: Int = 3
        d: Int = 4
      """)
    }
    'parsingProblems{
      check.session("""
        @ (1 + 1)
        res0: Int = 2

        @ (
        @ 1
        @ +
        @ 1
        @ )
        res1: Int = 2

        @ (
        @ (123.0).round
        @        .toChar
        @ )
        res2: Char = '{'

        @ Seq(0) map {_ + 1}
        res3: Seq[Int] = List(1)
      """)
    }
    'backticks{
      check.session("""
        @ val `1+1` = 1
        `1+1`: Int = 1

        @ val ++ = 1
        ++: Int = 1

        @ object `+1+`
        defined object `+1+`

        @ val ` ` = 1; type ` ` = Int
        ` `: Int = 1
        defined type ` `

        @ ((` ` + `1+1`): ` `): Int
        res4: Int = 2

        @ object ` `{ val ` ` = 333 }

        @ import ` `.` `

        @ ` `
        res7: Int = 333

        @ object ` `{ val ` ` = 123};
        defined object ` `

        @ import ` `.{` ` => `l o l`}

        @ `l o l`
        res10: Int = 123

        @ import ` `.{` ` => `//`}
        
        @ `//`
        res12: Int = 123

        @ import ` `.{` ` => `Something then //`}

        @ `Something then //`
        res14: Int = 123

        @ import ` `.{` ` => `// and then`}

        @ `// and then`
        res16: Int = 123

        @ import ` `.{` ` => `/*`}

        @ `/*`
        res18: Int = 123

        @ import ` `.{` ` => `/*/`}

        @ `/*/`
        res20: Int = 123

        @ import ` `.{` ` => `*/`}

        @ `*/`
        res22: Int = 123

        @ val `/*` = 123
        `/*`: Int = 123

        @ val `//` = 123
        `//`: Int = 123

      """)
    }
  }
}
