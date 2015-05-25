package ammonite.repl

import utest._

import scala.collection.{immutable => imm}

object AdvancedTests extends TestSuite{
  val tests = TestSuite{
    println("AdvancedTests")
    val check = new Checker()
    'load{
      'ivy{
        'standalone{
          val tq = "\"\"\""
          check.session(s"""
            @ import scalatags.Text.all._
            error: not found: value scalatags

            @ load.ivy("com.lihaoyi" %% "scalatags" % "0.4.5")

            @ import scalatags.Text.all._
            import scalatags.Text.all._

            @ a("omg", href:="www.google.com").render
            res2: String = $tq
            <a href="www.google.com">omg</a>
            $tq
          """)
        }
        'dependent{
          // Make sure it automatically picks up jawn-parser since upickle depends on it,
          check.session("""
            @ load.ivy("com.lihaoyi" %% "upickle" % "0.2.6")

            @ import upickle._
            import upickle._

            @ upickle.write(Seq(1, 2, 3))
            res2: String = "[1,2,3]"
          """)
        }

        'reloading{
          // Make sure earlier-loaded things indeed continue working
          check.session("""
            @ load.ivy("com.lihaoyi" %%"scalarx" % "0.2.7")

            @ load.ivy("com.scalatags" %% "scalatags" % "0.2.5")

            @ scalatags.all.div("omg").toString
            res2: String = "<div>omg</div>"

            @ load.ivy("com.lihaoyi" %% "scalatags" % "0.4.5")

            @ import scalatags.Text.all._; scalatags.Text.all.div("omg").toString
            import scalatags.Text.all._
            res4_1: String = "<div>omg</div>"

            @ import rx._; val x = Var(1); val y = Rx(x() + 1)

            @ x(); y()
            res6_0: Int = 1
            res6_1: Int = 2

            @ x() = 2

            @ x(); y()
            res8_0: Int = 2
            res8_1: Int = 3
          """)
        }
        'complex{
          check.session("""
            @ load.ivy("com.typesafe.akka" %% "akka-http-experimental" % "1.0-M3")

            @ implicit val system = akka.actor.ActorSystem()

            @ val serverBinding = akka.http.Http(system).bind(interface = "localhost", port = 31337)

            @ implicit val materializer = akka.stream.ActorFlowMaterializer()

            @ var set = false

            @ serverBinding.connections.runForeach { connection =>
            @   set = true
            @ }

            @ set
            res6: Boolean = false

            @ akka.stream.scaladsl.Source(
            @   List(akka.http.model.HttpRequest(uri="/"))
            @ ).via(
            @   akka.http.Http().outgoingConnection("localhost", port=31337).flow
            @ ).runForeach(println)

            @ Thread.sleep(200)

            @ set
            res9: Boolean = true

            @ system.shutdown()
          """)
        }
      }
      'code{
        check.session("""
          @ load("val x = 1")

          @ x
          res2: Int = 1
        """)
      }
    }
    'pprint{
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
    'exit{
      check.result("exit", Res.Exit)
    }
    'skip{
      check("1", "res0: Int = 1")
      check.result("", Res.Skip)
      check("2", "res1: Int = 2")
    }
    'history{
      check.session("""
        @ val x = 1

        @ x

        @ history
        res2: Seq[String] = Vector("val x = 1", "x")
      """)
    }
    'customPPrint{
      check.session("""
        @ class C
        defined class C

        @ implicit def pprint = ammonite.pprint.PPrinter[C]((t, c) => Iterator("INSTANCE OF CLASS C"))
        defined function pprint

        @ new C
        res2: C = INSTANCE OF CLASS C
      """)
    }

    'shapeless{
      check.session("""
        @ load.ivy("com.chuusai" %% "shapeless" % "2.1.0")

        @ import shapeless._

        @ (1 :: "lol" :: List(1, 2, 3) :: HNil)
        res2: Int :: String :: List[Int] :: HNil = ::(1, ::("lol", ::(List(1, 2, 3), HNil)))

        @ res2(1)
        res3: String = "lol"
      """)
    }

    'scalaz{
      check.session("""
        @ load.ivy("org.scalaz" %% "scalaz-core" % "7.1.1")

        @ import scalaz._
        import scalaz._

        @ import Scalaz._
        import Scalaz._

        @ (Option(1) |@| Option(2))(_ + _)
        res3: Option[Int] = Some(3)
      """)
    }
    'scalaparse{
      // Prevent regressions when wildcard-importing things called `macro` or `_`
      check.session("""
        @ import scalaparse.Scala._

        @ 1
        res1: Int = 1

        @ ExprCtx.Parened.parse("1 + 1")
        res2: fastparse.core.Result[Unit] = Failure(Parened:0 / "(":0 / "(":0 ..."1 + 1", false)

        @ ExprCtx.Parened.parse("(1 + 1)")
        res3: fastparse.core.Result[Unit] = Success((), 7)
      """)
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
    'customTypePrinter{
      check.session("""
        @ Array(1)
        res0: Array[Int] = Array(1)

        @ import ammonite.pprint.TPrint

        @ implicit def ArrayTPrint[T: TPrint]: TPrint[Array[T]] = TPrint.lambda(
        @   c => implicitly[TPrint[T]].render(c) + c.color.literal(" Array")
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
    'truncation{
      check.session("""
      @ Seq.fill(20)(100)
      res0: Seq[Int] = List(
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
      ...

      @ show(Seq.fill(20)(100))
      res1: ammonite.pprint.Show[Seq[Int]] = List(
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100,
        100
      )

      @ show(Seq.fill(20)(100), lines = 3)
      res2: ammonite.pprint.Show[Seq[Int]] = List(
        100,
        100,
      ...

      @ pprintConfig = pprintConfig.copy(lines = 5)

      @ Seq.fill(20)(100)
      res4: Seq[Int] = List(
        100,
        100,
        100,
        100,
      ...
      """)
    }
  }
}
