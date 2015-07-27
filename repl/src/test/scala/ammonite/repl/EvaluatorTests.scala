package ammonite.repl


import utest._
import acyclic.file

import scala.collection.{immutable => imm}
import scala.reflect.io.VirtualDirectory
import TestUtils.scala2_10

object EvaluatorTests extends TestSuite{

  val tests = TestSuite{
    println("EvaluatorTests")
    val check = new Checker()
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
      check.session("""
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
      check.session("""
        @ val x = Iterator.continually(1)
        x: Iterator[Int] = non-empty iterator

        @ val y = x.take(15)
        y: Iterator[Int] = non-empty iterator

        @ val z = y.foldLeft(0)(_ + _)
        z: Int = 15
      """)
    }

    'import{

      'basic {
        check.session("""
          @ import math.abs
          import math.abs

          @ val abs = 123L
          abs: Long = 123L

          @ abs
          res2: Long = 123L
        """)
      }
      'java {
        check.session("""
          @ import Thread._
          import Thread._

          @ currentThread.isAlive
          res1: Boolean = true

          @ import java.lang.Runtime.getRuntime
          import java.lang.Runtime.getRuntime

          @ getRuntime.isInstanceOf[Boolean]
          res3: Boolean = false

          @ getRuntime.isInstanceOf[java.lang.Runtime]
          res4: Boolean = true
        """)
      }
      'multi{
        check.session("""
          @ import math._, Thread._
          import math._, Thread._

          @ abs(-1)
          res1: Int = 1

          @ currentThread.isAlive
          res2: Boolean = true
        """)
      }
      'shadowing{
        check.session("""
          @ val abs = 'a'
          abs: Char = 'a'

          @ abs
          res1: Char = 'a'

          @ val abs = 123L
          abs: Long = 123L

          @ abs
          res3: Long = 123L

          @ import math.abs
          import math.abs

          @ abs(-10)
          res5: Int = 10

          @ val abs = 123L
          abs: Long = 123L

          @ abs
          res7: Long = 123L

          @ import java.lang.Math._
          import java.lang.Math._

          @ abs(-4)
          res9: Int = 4
        """)
      }
      'renaming{
        check.session("""
          @ import math.{abs => sba}

          @ sba(-123)
          res1: Int = 123

          @ abs
          error: not found: value abs

          @ import math.{abs, max => xam}

          @ abs(-234)
          res3: Int = 234

          @ xam(1, 2)
          res4: Int = 2

          @ import math.{min => _, _}

          @ max(2, 3)
          res6: Int = 3

          @ min
          error: not found: value min
        """)
      }
    }


    'classes{
      check.session("""
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

        @ CO
        res8: CO.type = CO

        @ CO()
        res9: CO = CO()
      """)
    }

    'packageImport{
      check.session("""
        @ import pprint._

        @ import Config.Defaults._
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
      check.session("""
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
      """)
    }
  }
}
