package ammonite.kernel

import KernelTests._
import org.scalatest.FreeSpec

class ImportTests extends FreeSpec {

  val kernel = buildKernel()

  "basic" - {
    "hello" in {
      checkSuccess(kernel,
                   Vector(
                     ("import math.abs", checkUnit),
                     ("val abs = 123L", checkUnit),
                     ("abs", checkLong(123L))
                   ))
    }
    "java" in {
      checkSuccess(kernel,
                   Vector(
                     ("import Thread._", checkUnit),
                     ("currentThread.isAlive", checkBoolean(true)),
                     ("import java.lang.Runtime.getRuntime", checkUnit),
                     ("getRuntime.isInstanceOf[Boolean]", checkBoolean(false)),
                     ("getRuntime.isInstanceOf[java.lang.Runtime]", checkBoolean(true))
                   ))
    }
    "multi" in {
      checkSuccess(kernel,
                   Vector(
                     ("import math._, Thread._", checkUnit),
                     ("abs(-1)", checkInt(1)),
                     ("currentThread.isAlive", checkBoolean(true))
                   ))
    }
    "renaming" in {
      checkSuccess(kernel,
                   Vector(
                     ("import math.{abs => sba}", checkUnit),
                     ("sba(-123)", checkInt(123)),
                     ("import math.{abs, max => xam}", checkUnit),
                     ("abs(-234)", checkInt(234)),
                     ("xam(1, 2)", checkInt(2)),
                     ("import math.{min => _, _}", checkUnit),
                     ("max(2, 3)", checkInt(3))
                   ))
    }
  }
  "shadowing" - {
    "sameName" in {
      checkSuccess(kernel,
                   Vector(
                     ("val abs = 'a'", checkUnit),
                     ("abs", checkChar('a')),
                     ("val abs = 123L", checkUnit),
                     ("abs", checkLong(123L)),
                     ("import math.abs", checkUnit),
                     ("abs(-10)", checkInt(10)),
                     ("val abs = 123L", checkUnit),
                     ("abs", checkLong(123L)),
                     ("import java.lang.Math._", checkUnit),
                     ("abs(-4)", checkInt(4))
                   ))
    }
    "shadowPrefix" in {
      checkSuccess(kernel,
                   Vector(
                     ("object baz {val foo = 1}", checkUnit),
                     ("object foo {val bar = 2}", checkUnit),
                     ("import foo.bar", checkUnit),
                     ("import baz.foo", checkUnit),
                     ("bar", checkInt(2))
                   ))
    }

    "typeTermSeparation" - {
      // Make sure that you can have a term and a type of the same name
      // coming from different places and they don't stomp over each other
      // (#199) and both are accessible.
      // "case1" in check.session(s"""
      //     @ val Foo = 1

      //     @ type Foo = Int

      //     @ Foo
      //     res2: Int = 1

      //     @ 2: Foo
      //     res3: ${sessionPrefix}Foo = 2
      //   """)

      // "case2" in check.session(s"""
      //       @ object pkg1{ val Order = "lolz" }

      //       @ object pkg2{ type Order[+T] = Seq[T] }

      //       @ import pkg1._

      //       @ Order
      //       res3: String = "lolz"

      //       @ import pkg2._

      //       @ Seq(1): Order[Int]
      //       res5: Order[Int] = List(1)

      //       @ Seq(Order): Order[String]
      //       res6: Order[String] = List("lolz")
      //     """)

      // Even though you can import the same-named type and term from different
      // places and have it work, if you import them both from the same place,
      // a single type or term of the same name will stomp over both of them.
      //
      // This is basically impossible to avoid in Scala unless we want to
      // generate forwarder objects to import from which is very difficult
      // (e.g. we would need to generate forwarder-methods for arbitrarily
      // complex method signatures) or generate ever-more-nested wrapper
      // objects for imports to make the later imports take priority (which
      // results in a quadratic number of class files)
      //
      // This is sufficiently edge-casey that I'm gonna call this a wontfix
      // "case3" in check.session("""
      //     @ object bar { val foo = 1; type foo = Int }

      //     @ object baz { val foo = 2 }

      //     @ import bar.foo

      //     @ import baz.foo

      //     @ foo
      //     res4: Int = 2

      //     @ 1: foo
      //     error: Compilation Failed
      //   """)

      // "paulp" in {

      //   check.session(s"""
      //     @ import ammonite.testcode.paulp1._, ammonite.testcode.paulp2._

      //     @ new Paulp; Paulp // Paulp's example in #199
      //     res1_0: Paulp = paulp1.Paulp1
      //     res1_1: Paulp.type = paulp2.Paulp2

      //     @ val Paulp = 123 // Shadow the term but not the type

      //     @ new Paulp; Paulp // Shouldn't change
      //     res3_0: Paulp = paulp1.Paulp1
      //     res3_1: Int = 123

      //     @ object Paulp3{
      //     @   val Paulp = 1
      //     @   type Paulp = Array[Int]
      //     @ }

      //     @ import Paulp3._ // Actually shadow them now

      //     @ (new Paulp(0)).length; Paulp
      //     res6_0: Int = 0
      //     res6_1: Int = 1

      //     @ object Paulp4{ object Paulp{override def toString = "Paulp4"}}

      //     @ object Paulp5{ class Paulp{override def toString = "Paulp5"}}

      //     @ import Paulp4.Paulp, Paulp5.Paulp // cross import, shadow both

      //     @ Paulp
      //     res10: Paulp.type = Paulp4

      //     @ new Paulp
      //     res11: Paulp = Paulp5

      //     @ import ammonite.testcode.paulp1._ // individually import & shadow...

      //     @ new Paulp; Paulp
      //     res13_0: Paulp = paulp1.Paulp1
      //     res13_1: Paulp.type = Paulp4

      //     @ import ammonite.testcode.paulp2._ // one at a time...

      //     @ new Paulp; Paulp
      //     res15_0: Paulp = paulp1.Paulp1
      //     res15_1: Paulp.type = paulp2.Paulp2

      //     @ object Paulp6{ val Paulp = 1; type Paulp = Int }

      //     @ import Paulp6._ // This should shadow both

      //     @ Paulp: Paulp
      //     res18: Paulp = 1

      //     @ import Paulp4._

      //     @ Paulp
      //     res20: Paulp.type = Paulp4

      //     @ Paulp: Paulp // Improper shadowing! This is a known issue but hard to fix
      //     error: not found: type Paulp

      //     @ val Paulp = 12345

      //     @ import Paulp6.Paulp

      //     @ Paulp
      //     """)
      // }
      // "paulpTypeRegression" in {
      //   check.session(s"""
      //     @ type Paulp = Int

      //     @ import ammonite.testcode.paulp3.Paulp

      //     @ new Paulp
      //     res2: Paulp = paulp3.Paulp-class
      //   """)
      // }
    }
  }
  "collapsing" - {
    checkSuccess(kernel,
                 Vector(
                   ("object Foo{val bar = 1}", checkUnit),
                   ("import Foo.bar", checkUnit),
                   ("import Foo.{bar => _}", checkUnit),
                   ("bar", checkInt(1))
                 ))
  }

}
