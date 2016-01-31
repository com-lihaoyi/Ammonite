package ammonite.repl.session

import ammonite.repl.Checker
import utest._

import scala.collection.{immutable => imm}

object ImportTests extends TestSuite{

  val tests = TestSuite{
    println("ImportTests")
    val check = new Checker()

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
        check.session("""
          @ object importing_issue {
          @   object scala {
          @     def evilThing = ???
          @   }
          @ }

          @ import scala.concurrent.duration._

          @ Duration.Inf

          @ import importing_issue._

          @ Duration.Inf

          @ "foo"
          res5: String = "foo"
        """)
      }
      'shadowPrefix2{
        // This failed kinda non-deterministically in 0.5.2; it depended
        // on what ordering the imports were generated in, which depended
        // on the ordering of the `scala.Map` they were stored in, which
        // is arbitrary. Choosing different identifiers for `foo` `bar` and
        // `baz` affected this ordering and whether or not it fail.
        // Nevertheless, here's a test case that used to fail, but now doesn't
        check.session("""
          @ object baz { val foo = 1 }

          @ object foo { val bar = 2 }

          @ import foo.bar

          @ import baz.foo

          @ bar
          res4: Int = 2
        """)
      }
    }
  }
}
