package ammonite.repl

import utest._

import scala.collection.{immutable => imm}

object ProjectTests extends TestSuite{
  val tests = TestSuite{
    println("ProjectTests")
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
      if (!scala.util.Properties.versionString.contains("2.10"))
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
      if (!scala.util.Properties.versionString.contains("2.10"))
        check.session(s"""
          @ import scalaparse.Scala._

          @ 1
          res1: Int = 1

          @ ExprCtx.Parened.parse("1 + 1") // for some reason the tuple isn't pprinted
          res2: fastparse.core.Result[Unit] = Failure("1 + 1", 0, "(", (0, Parened))

          @ ExprCtx.Parened.parse("(1 + 1)")
          res3: fastparse.core.Result[Unit] = Success((), 7)
        """)
      else
        check.session(s"""
          @ import scalaparse.Scala._

          @ 1
          res1: Int = 1

          @ ExprCtx.Parened.parse("1 + 1") // for some reason the tuple isn't pprinted
          res2: fastparse.core.Result[Unit] = Failure("(":0 ..."1 + 1")

          @ ExprCtx.Parened.parse("(1 + 1)")
          res3: fastparse.core.Result[Unit] = Success((),7)
        """)
    }

    'finagle{
      // Prevent regressions when wildcard-importing things called `macro` or `_`
      check.session("""
        @ load.ivy("com.twitter" %% "finagle-httpx" % "6.26.0")

        @ import com.twitter.finagle.{Httpx, Service}

        @ import com.twitter.finagle.httpx

        @ import com.twitter.util.{Await, Future}

        @ var serverCount = 0

        @ var clientResponse = 0

        @ val service = new Service[httpx.Request, httpx.Response] {
        @   def apply(req: httpx.Request): Future[httpx.Response] = {
        @     serverCount += 1
        @     Future.value(
        @       httpx.Response(req.version, httpx.Status.Ok)
        @     )
        @   }
        @ }

        @ val server = Httpx.serve(":8080", service)

        @ val client: Service[httpx.Request, httpx.Response] = Httpx.newService(":8080")

        @ val request = httpx.Request(httpx.Method.Get, "/")

        @ request.host = "www.scala-lang.org"

        @ val response: Future[httpx.Response] = client(request)

        @ response.onSuccess { resp: httpx.Response =>
        @   clientResponse = resp.getStatusCode
        @ }

        @ Await.ready(response)

        @ serverCount
        res14: Int = 1

        @ clientResponse
        res15: Int = 200

        @ server.close()
      """)
    }
    'spire{
      // Prevent regressions when wildcard-importing things called `macro` or `_`
      if (!scala.util.Properties.versionString.contains("2.10"))
        check.session(s"""
          @ load.ivy("org.spire-math" %% "spire" % "0.10.1")

          @ import spire.implicits._

          @ import spire.math._

          @ def euclidGcd[A: Integral](x: A, y: A): A = {
          @   if (y == 0) x
          @   else euclidGcd(y, x % y)
          @ }

          @ euclidGcd(42, 96)
          res4: Int = 6

          @ euclidGcd(42L, 96L)
          res5: Long = 6L

          @ euclidGcd(BigInt(42), BigInt(96))
          res6: BigInt = 6

          @ def mean[A: Fractional](xs: A*): A = xs.reduceLeft(_ + _) / xs.size

          @ mean(0.5, 1.5, 0.0, -0.5)
          res8: Double = 0.375

          @ mean(Rational(1, 2), Rational(3, 2), Rational(0))
          res9: Rational = 2/3
        """)
      else
        check.session(s"""
          @ load.ivy("org.spire-math" %% "spire" % "0.10.1")

          @ import spire.implicits._

          @ import spire.math._

          @ def euclidGcd[A: Integral](x: A, y: A): A = {
          @   if (y == 0) x
          @   else euclidGcd(y, x % y)
          @ }

          @ euclidGcd(42, 96)
          res4: Int = 6

          @ euclidGcd(42L, 96L)
          res5: Long = 6L

          @ euclidGcd(BigInt(42), BigInt(96))
          res6: math.BigInt = 6

          @ def mean[A: Fractional](xs: A*): A = xs.reduceLeft(_ + _) / xs.size

          @ mean(0.5, 1.5, 0.0, -0.5)
          res8: Double = 0.375

          @ mean(Rational(1, 2), Rational(3, 2), Rational(0))
          res9: spire.math.Rational = 2/3
        """)


    }

  }
}

