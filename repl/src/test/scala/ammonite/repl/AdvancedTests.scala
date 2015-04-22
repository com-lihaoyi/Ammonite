package ammonite.repl

import utest._

import scala.collection.{immutable => imm}

object AdvancedTests extends TestSuite{
  val tests = TestSuite{
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
        'resolvers{
          // Actually tests that the Resolver._ methods can be called, does not try to fetch artifacts from them
          check.session("""
            @ load.resolver(Resolver.sonatypeRepo("snapshots"))
            res0: Unit = ()

            @ load.resolver(Resolver.mavenResolver("Scalaz Bintray Repo", "https://dl.bintray.com/scalaz/releases"))
            res1: Unit = ()

            @ load.resolver(Resolver.mavenLocal)
            res2: Unit = ()

            @ load.resolver(Resolver.defaultMaven)
            res3: Unit = ()

            @ load.resolver(Resolver.localRepo)
            res4: Unit = ()
                        """)
        }

        'reloading{
          // Make sure earlier-loaded things indeed continue working
          check.session("""
            @ load.ivy("com.lihaoyi" %%"scalarx" % "0.2.7")

            @ load.ivy("com.scalatags" %% "scalatags" % "0.2.5")

            @ scalatags.all.div("omg").toString
            res2: java.lang.String = "<div>omg</div>"

            @ load.ivy("com.lihaoyi" %% "scalatags" % "0.4.5")

            @ import scalatags.Text.all._; scalatags.Text.all.div("omg").toString
            import scalatags.Text.all._
            res4_1: java.lang.String = "<div>omg</div>"

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
        res0: Seq[Seq[java.lang.String]] = List(
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
        res2: cmd1.Foo = Foo(1, "", List())

        @ Foo(1234567, "I am a cow, hear me moo", Seq("I weigh twice as much as you", "and I look good on the barbecue"))
        res3: cmd1.Foo = Foo(
          1234567,
          "I am a cow, hear me moo",
          List("I weigh twice as much as you", "and I look good on the barbecue")
        )
      """)
    }
    'multiline{
      check.result("{ 1 +", Res.Buffer("{ 1 +"))
      check("1 }", "res0: Int = 2")
      check.result("(", Res.Buffer("("))
      check.result("1", Res.Buffer("(\n1"))
      check.result("+", Res.Buffer("(\n1\n+"))
      check.result("2", Res.Buffer("(\n1\n+\n2"))
      check(")", "res1: Int = 3")
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
        res2: scala.Seq[String] = Vector("val x = 1", "x")
      """)
    }
    'customPPrint{
      check.session("""
        @ class C
        defined class C

        @ implicit def pprint = ammonite.pprint.PPrinter[C]((t, c) => Iterator("INSTANCE OF CLASS C"))
        defined function pprint

        @ new C
        res2: cmd0.C = INSTANCE OF CLASS C
      """)
    }

    'shapeless{
      check.session("""
        @ load.ivy("com.chuusai" %% "shapeless" % "2.1.0")

        @ import shapeless._

        @ (1 :: "lol" :: List(1, 2, 3) :: HNil)(1)
        res2: java.lang.String = "lol"
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
        res3: scala.Option[Int] = Some(3)
      """)
    }

    'scalazstream{
      check.session("""
        @ load.resolver("Scalaz Bintray Repo" at "https://dl.bintray.com/scalaz/releases")

        @ load.ivy("org.scalaz.stream" %% "scalaz-stream" % "0.7a")

        @ import scalaz.stream._
        import scalaz.stream._

        @ import scalaz.concurrent.Task
        import scalaz.concurrent.Task

        @ val p1 = Process.constant(1).toSource
        p1: scalaz.stream.Process[scalaz.concurrent.Task,Int] = Append(Emit(Vector(1)),Vector(<function1>))

        @ val pch = Process.constant((i:Int) => Task.now(())).take(3)
        pch: scalaz.stream.Process[Nothing,Int => scalaz.concurrent.Task[Unit]] = Append(Halt(End),Vector(<function1>))

        @ p1.to(pch).runLog.run.size == 3
        res6: Boolean = true
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
        res1: java.lang.String = "2"

        @ x + y
        res2: String = "12"

        @ abs(-x)
        res3: Int = 1
      """)

    }
  }
}
