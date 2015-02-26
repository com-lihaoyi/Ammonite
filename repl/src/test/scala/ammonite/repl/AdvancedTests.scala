package ammonite.repl

import utest._

import scala.collection.{immutable => imm}

object AdvancedTests extends TestSuite{
  val tests = TestSuite{
    val check = new Checker()
    'load{
      'ivy{
        'standalone{
          check.fail("import scalatags.Text.all._", _.contains("not found: value scalatags"))
          check("""load.ivy("com.lihaoyi" %% "scalatags" % "0.4.5")""")
          check("import scalatags.Text.all._", "import scalatags.Text.all._")
          check(
            """a("omg", href:="www.google.com").render""",
            """res2: String = "<a href=\"www.google.com\">omg</a>""""
          )
        }
        'dependent{
          // Make sure it automatically picks up jawn-parser since upickle depends on it,
          check("""load.ivy("com.lihaoyi", "upickle_2.11", "0.2.6")""")
          check("import upickle._")
          check("upickle.write(Seq(1, 2, 3))", """res2: String = "[1,2,3]"""")
        }

        'reloading{
          // Make sure earlier-loaded things indeed continue working
          check("""load.ivy("com.lihaoyi" %%"scalarx" % "0.2.7")""")
          check("""load.ivy("com.scalatags" %% "scalatags" % "0.2.5")""")
          check(
            """scalatags.all.div("omg").toString""",
            """res2: java.lang.String = "<div>omg</div>""""
          )
          check("""load.ivy("com.lihaoyi" %% "scalatags" % "0.4.5")""")
          check(
            """import scalatags.Text.all._; scalatags.Text.all.div("omg").toString""",
            """import scalatags.Text.all._
              |res4_1: java.lang.String = "<div>omg</div>"""".stripMargin
          )
          check("""import rx._; val x = Var(1); val y = Rx(x() + 1)""")
          check("""x(); y()""", "res6_0: Int = 1\nres6_1: Int = 2")
          check("""x() = 2""")
          check("""x(); y()""", "res8_0: Int = 2\nres8_1: Int = 3")
        }
        'complex{
          check("""load.ivy("com.typesafe.akka" %% "akka-http-experimental" % "1.0-M3")""")
          check("""implicit val system = akka.actor.ActorSystem()""")
          check("""val serverBinding = akka.http.Http(system).bind(interface = "localhost", port = 31337)""")
          check("""implicit val materializer = akka.stream.ActorFlowMaterializer()""")
          check("""var set = false""")
          check("""
            serverBinding.connections.runForeach { connection =>
              set = true
            }
          """)
          check("""set""", "res6: Boolean = false")
          check("""
            akka.stream.scaladsl.Source(
              List(akka.http.model.HttpRequest(uri="/"))
            ).via(
              akka.http.Http().outgoingConnection("localhost", port=31337).flow
            ).runForeach(println)
          """)
          check("""Thread.sleep(200)""")
          check("""system.shutdown()""")
          check("""set""", "res10: Boolean = true")
        }
      }
      'code{
        check("""load("val x = 1")""")
        check("""x""", "res2: Int = 1")
      }
    }
    'multiline{
      check.result("{ 1 +", Result.Buffer("{ 1 +"))
      check("1 }", "res0: Int = 2")
      check.result("(", Result.Buffer("("))
      check.result("1", Result.Buffer("(\n1"))
      check.result("+", Result.Buffer("(\n1\n+"))
      check.result("2", Result.Buffer("(\n1\n+\n2"))
      check(")", "res1: Int = 3")
    }
    'exit{
      check.result("exit", Result.Exit)
    }
    'skip{
      check("1", "res0: Int = 1")
      check.result("", Result.Skip)
      check("2", "res1: Int = 2")
    }
    'history{
      check("""val x = 1""")
      check("x")
      check("history", """res2: scala.Seq[String] = Vector("val x = 1", "x")""")
    }
  }
}
