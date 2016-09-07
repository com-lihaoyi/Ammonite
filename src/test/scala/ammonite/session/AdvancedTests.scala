package ammonite.session

import ammonite.TestUtils._
import ammonite.TestRepl
import ammonite.util.Res
import org.scalatest.FreeSpec

import ammonite.ops._

class AdvancedTests extends FreeSpec {

  def check = new TestRepl()

  "pprint" in {
    check.session(s"""
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
        res2: ${sessionPrefix}Foo = Foo(1, "", List())

        @ Foo(
        @   1234567,
        @   "I am a cow, hear me moo",
        @   Seq("I weigh twice as much as you", "and I look good on the barbecue")
        @ )
        res3: ${sessionPrefix}Foo = Foo(
          1234567,
          "I am a cow, hear me moo",
          List("I weigh twice as much as you", "and I look good on the barbecue")
        )
      """)
  }

  "exit" in {
    check.result("exit", Res.Exit())
  }

  "skip" in {
    check.result("", Res.Skip)
  }

  "predef" in {
    val check2 = new TestRepl {
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

  "predefSettings" in {
    val check2 = new TestRepl {
      override def predef = """
          repl.compiler.settings.Xexperimental.value = true
        """
    }
    check2.session("""
        @ repl.compiler.settings.Xexperimental.value
        res0: Boolean = true
      """)

  }

  "macros" in {
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

  "typeScope" in {
    check.session("""
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

  "customTypePrinter" in {
    check.session("""
        @ Array(1)
        res0: Array[Int] = Array(1)

        @ import pprint.TPrint

        @ implicit def ArrayTPrint[T: TPrint]: TPrint[Array[T]] = TPrint.lambda( c =>
        @   implicitly[TPrint[T]].render(c) +
        @   " " +
        @   c.typeColor("Array").render
        @ )

        @ Array(1)
        res3: Int Array = Array(1)
      """)
  }

  "unwrapping" in {
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

  "forceWrapping" in {
    check.session("""
        @ {{
        @   val x = 1
        @   val y = 2
        @   x + y
        @ }}
        res0: Int = 3
      """)
  }

  // 'truncation {
  //   // Need a way to capture stdout in tests to make these tests work
  //   if (false)
  //     check.session("""
  //       @ Seq.fill(20)(100)
  //       res0: Seq[Int] = List(
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //       ...

  //       @ show(Seq.fill(20)(100))
  //       res1: ammonite.pprint.Show[Seq[Int]] = List(
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100,
  //         100
  //       )

  //       @ show(Seq.fill(20)(100), height = 3)
  //       res2: ammonite.pprint.Show[Seq[Int]] = List(
  //         100,
  //         100,
  //       ...

  //       @ pprintConfig() = pprintConfig().copy(height = 5 )

  //       @ Seq.fill(20)(100)
  //       res4: Seq[Int] = List(
  //         100,
  //         100,
  //         100,
  //         100,
  //       ...
  //     """)
  // }

  "private" in {
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

  "compilerPlugin" in {
    check.session("""
        @ // Make sure plugins from eval class loader are not loaded

        @ import $ivy.`org.spire-math::kind-projector:0.6.3`

        @ trait TC0[F[_]]
        defined trait TC0

        @ type TC0EitherStr = TC0[Either[String, ?]]
        error: not found: type ?

        @ // This one must be loaded

        @ import $plugin.$ivy.`org.spire-math::kind-projector:0.6.3`

        @ trait TC[F[_]]
        defined trait TC

        @ type TCEitherStr = TC[Either[String, ?]]
        defined type TCEitherStr

        @ // Useless - does not add plugins, and ignored by eval class loader

        @ import $plugin.$ivy.`com.lihaoyi::scalatags:0.4.5`

        @ import scalatags.Text
        error: not found: value scalatags
      """)
  }

  "desugar" in {
    check.session("""
        @ desugar{1 + 2 max 3}
        res0: Desugared = scala.Predef.intWrapper(3).max(3)
      """)
  }

  "loadingModulesInPredef" - {
    val dir = pwd / 'src / 'test / 'resources / 'scripts / 'predefWithLoad
    "loadExec"  in {
      val c1 = new TestRepl() {
        override def predef = read ! dir / "PredefLoadExec.sc"
      }
      c1.session("""
            @ val previouslyLoaded = predefDefinedValue
            previouslyLoaded: Int = 1337
          """)
    }
    "loadModule" in {
      val c2 = new TestRepl() {
        override def predef = read ! dir / "PredefLoadModule.sc"
      }
      c2.session("""
            @ val previouslyLoaded = 1337
            previouslyLoaded: Int = 1337
          """)
    }
  }

}
