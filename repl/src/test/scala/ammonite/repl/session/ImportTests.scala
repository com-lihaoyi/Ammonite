package ammonite.repl.session

import ammonite.repl.TestRepl
import ammonite.repl.TestUtils._
import utest._

import scala.collection.{immutable => imm}

object ImportTests extends TestSuite{

  val tests = TestSuite{
    println("ImportTests")
    val check = new TestRepl()

    'basic {
      'hello{
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
    'shadowing{
      'sameName{
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
      'shadowPrefix{
        * - check.session("""
          @ object importing_issue {
          @   object scala {
          @     def evilThing = ???
          @   }
          @ }

          @ import scala.concurrent.duration._

          @ Duration.Inf

          @ import importing_issue._

          @ "foo" // Make sure this still works
          res4: String = "foo"

          @ Duration.Inf // This fails due to a compiler bug SI-6039
          error: Compilation Failed
        """)
        // This failed kinda non-deterministically in 0.5.2 (#248); it depended
        // on what ordering the imports were generated in, which depended
        // on the ordering of the `scala.Map` they were stored in, which
        // is arbitrary. Choosing different identifiers for `foo` `bar` and
        // `baz` affected this ordering and whether or not it fail.
        // Nevertheless, here's a test case that used to fail, but now doesn't
        * - check.session("""
          @ object baz { val foo = 1 }

          @ object foo { val bar = 2 }

          @ import foo.bar

          @ import baz.foo

          @ bar
          res4: Int = 2
        """)

      }

      'typeTermSeparation{
        // Make sure that you can have a term and a type of the same name
        // coming from different places and they don't stomp over each other
        // (#199) and both are accessible.
        * - check.session("""
          @ val Foo = 1

          @ type Foo = Int

          @ Foo
          res2: Int = 1

          @ 2: Foo
          res3: Foo = 2
        """)

        * - {
          val pkg2 = if (scala2_10) "pkg2." else ""
          check.session(s"""
            @ object pkg1{ val Order = "lolz" }

            @ object pkg2{ type Order[+T] = Seq[T] }

            @ import pkg1._

            @ Order
            res3: String = "lolz"

            @ import pkg2._

            @ Seq(1): Order[Int]
            res5: ${pkg2}Order[Int] = List(1)

            @ Seq(Order): Order[String]
            res6: ${pkg2}Order[String] = List("lolz")
          """)
        }

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
        * - check.session("""
          @ object bar { val foo = 1; type foo = Int }

          @ object baz { val foo = 2 }

          @ import bar.foo

          @ import baz.foo

          @ foo
          res4: Int = 2

          @ 1: foo
          error: Compilation Failed
        """)

        'paulp - check.session("""
          @ import ammonite.repl.testcode.paulp1._, ammonite.repl.testcode.paulp2._

          @ new Paulp; Paulp // Paulp's example in #199
          res1_0: Paulp = paulp1.Paulp1
          res1_1: Paulp.type = paulp2.Paulp2

          @ val Paulp = 123 // Shadow the term but not the type

          @ new Paulp; Paulp // Shouldn't change
          res3_0: Paulp = paulp1.Paulp1
          res3_1: Int = 123

          @ object Paulp3{
          @   val Paulp = 1
          @   type Paulp = Array[Int]
          @ }

          @ import Paulp3._ // Actually shadow them now

          @ (new Paulp(0)).length; Paulp
          res6_0: Int = 0
          res6_1: Int = 1

          @ object Paulp4{ object Paulp{override def toString = "Paulp4"}}

          @ object Paulp5{ class Paulp{override def toString = "Paulp5"}}

          @ import Paulp4.Paulp, Paulp5.Paulp // cross import, shadow both

          @ Paulp
          res10: Paulp.type = Paulp4

          @ new Paulp
          res11: Paulp = Paulp5

          @ import ammonite.repl.testcode.paulp1._ // individually import & shadow...

          @ new Paulp; Paulp
          res13_0: Paulp = paulp1.Paulp1
          res13_1: Paulp.type = Paulp4

          @ import ammonite.repl.testcode.paulp2._ // one at a time...

          @ new Paulp; Paulp
          res15_0: Paulp = paulp1.Paulp1
          res15_1: Paulp.type = paulp2.Paulp2

          @ object Paulp6{ val Paulp = 1; type Paulp = Int }

          @ import Paulp6._ // This should shadow both

          @ Paulp: Paulp
          res18: Paulp = 1

          @ import Paulp4._

          @ Paulp
          res20: Paulp.type = Paulp4

          @ Paulp: Paulp // Improper shadowing!
          error: not found: type Paulp

          @ val Paulp = 12345

          @ import Paulp6.Paulp

          @ Paulp
        """)
      }
      'paulpTypeRegression{
        check.session("""
          @ type Paulp = Int

          @ import ammonite.repl.testcode.paulp3.Paulp

          @ new Paulp
          res2: Paulp = paulp3.Paulp-class
        """)
      }
    }
  }
}
