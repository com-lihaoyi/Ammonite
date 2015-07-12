package ammonite.repl

import utest._

import scala.collection.{immutable => imm}

object ProjectTests extends TestSuite{
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
        res2: fastparse.core.Result[Unit] = Failure("1 + 1", 0, "(", (0, Parened))

        @ ExprCtx.Parened.parse("(1 + 1)")
        res3: fastparse.core.Result[Unit] = Success((), 7)
      """)
    }
  }
}
