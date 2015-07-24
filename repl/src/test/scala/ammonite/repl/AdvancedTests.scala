package ammonite.repl

import utest._

import scala.collection.{immutable => imm}
import acyclic.file
object AdvancedTests extends TestSuite{
  val tests = TestSuite{
    println("AdvancedTests")
    val check = new Checker()
    'pprint{
      if (!scala.util.Properties.versionString.contains("2.10")) //buggy in 2.10
      check.session("""
        @ Seq.fill(10)(Seq.fill(3)("Foo"))
        res0: Seq[Seq[String]] = List(
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo"),
          List("Foo", "Foo", "Foo")
        )

        @ case class Foo(i: Int, s0: String, s1: Seq[String])
        defined class Foo

        @ Foo(1, "", Nil)
        res2: Foo = Foo(1, "", List())

        @ Foo(1234567, "I am a cow, hear me moo", Seq("I weigh twice as much as you", "and I look good on the barbecue"))
        res3: Foo = Foo(
          1234567,
          "I am a cow, hear me moo",
          List("I weigh twice as much as you", "and I look good on the barbecue")
        )
      """)
    }
    'specialPPrint{
      // Make sure these various "special" data structures get pretty-printed
      // correctly, i.e. not as their underlying type but as something more
      // pleasantly human-readable
      if (!scala.util.Properties.versionString.contains("2.10"))
        check.session("""
          @ import ammonite.ops._

          @ ls! wd/'ops
          res1: LsSeq = LsSeq(
            'src,
            'target
          )

          @ %%ls 'ops
          res2: CommandResult =
          src
          target
        """)
      else
        check.session("""
          @ import ammonite.ops._

          @ ls! wd/'ops
          res1: ammonite.ops.LsSeq = LsSeq(
            'src,
            'target
          )

          @ %%ls 'ops
          res2: ammonite.ops.CommandResult =
          src
          target
        """)
    }
    'exit{
      check.result("exit", Res.Exit)
    }
    'skip{
      check.result("", Res.Skip)
    }

    'predef{
      val check2 = new Checker{
        override def predef = """
          import math.abs
          val x = 1
          val y = "2"
        """
      }
      check2.session("""
        @ -x
        res0: Int = -1

        @ y
        res1: String = "2"

        @ x + y
        res2: String = "12"

        @ abs(-x)
        res3: Int = 1
      """)

    }
    'macros{
      check.session("""
        @ import language.experimental.macros

        @ import reflect.macros.Context

        @ def impl(c: Context): c.Expr[String] = {
        @  import c.universe._
        @  c.Expr[String](Literal(Constant("Hello!")))
        @ }
        defined function impl

        @ def m: String = macro impl
        defined function m

        @ m
        res4: String = "Hello!"
      """)
    }
    'typeScope{
      // Fancy type-printing isn't implemented at all in 2.10.x
      if (!scala.util.Properties.versionString.contains("2.10")) check.session("""
        @ collection.mutable.Buffer(1)
        res0: collection.mutable.Buffer[Int] = ArrayBuffer(1)

        @ import collection.mutable

        @ collection.mutable.Buffer(1)
        res2: mutable.Buffer[Int] = ArrayBuffer(1)

        @ mutable.Buffer(1)
        res3: mutable.Buffer[Int] = ArrayBuffer(1)

        @ import collection.mutable.Buffer

        @ mutable.Buffer(1)
        res5: Buffer[Int] = ArrayBuffer(1)
      """)
    }
    'customTypePrinter{
      check.session("""
        @ Array(1)
        res0: Array[Int] = Array(1)

        @ import ammonite.repl.frontend.TPrint

        @ implicit def ArrayTPrint[T: TPrint]: TPrint[Array[T]] = TPrint.lambda( c =>
        @   implicitly[TPrint[T]].render(c) +
        @   " " +
        @   c.colors.literalColor +
        @   "Array" +
        @   c.colors.endColor
        @ )

        @ Array(1)
        res3: Int Array = Array(1)
      """)
    }
    'unwrapping{
      check.session("""
        @ {
        @   val x = 1
        @   val y = 2
        @   x + y
        @ }
        x: Int = 1
        y: Int = 2
        res0_2: Int = 3
      """)
    }
    'forceWrapping{
      check.session("""
        @ {{
        @   val x = 1
        @   val y = 2
        @   x + y
        @ }}
        res0: Int = 3
      """)
    }
    'private{
      check.session("""
        @ private val x = 1; val y = x + 1
        x: Int = 1
        y: Int = 2

        @ y
        res1: Int = 2

        @ x
        error: not found: value x
      """)
    }
  }
}
