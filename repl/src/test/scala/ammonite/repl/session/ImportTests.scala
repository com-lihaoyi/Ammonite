package ammonite.repl.session

import ammonite.repl.Checker
import utest._

import scala.collection.{immutable => imm}

object ImportTests extends TestSuite{

  val tests = TestSuite{
    println("ImportTests")
    val check = new Checker()

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
}
