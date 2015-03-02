package ammonite.repl

import utest._

import scala.collection.{immutable => imm}

object AdvancedTests extends TestSuite{
  val tests = TestSuite{
    val check = new Checker()
    'load{
      'ivy{
        'standalone{
          check.session("""
            @ import scalatags.Text.all._
            error: not found: value scalatags

            @ load.ivy("com.lihaoyi" %% "scalatags" % "0.4.5")

            @ import scalatags.Text.all._
            import scalatags.Text.all._

            @ a("omg", href:="www.google.com").render
            res2: String = "<a href=\"www.google.com\">omg</a>"
          """)
        }
        'dependent{
          // Make sure it automatically picks up jawn-parser since upickle depends on it,
          check.session("""
            @ load.ivy("com.lihaoyi", "upickle_2.11", "0.2.6")

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
  }
}
