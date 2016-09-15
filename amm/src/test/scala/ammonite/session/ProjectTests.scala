package ammonite.session

import ammonite.TestRepl
import ammonite.TestUtils._
import utest._

import scala.collection.{immutable => imm}

object ProjectTests extends TestSuite{
  val tests = TestSuite{
    println("ProjectTests")
    val check = new TestRepl()
    'load {
      'ivy {
        'standalone - {
          retry(3) {
            // ivy or maven central are flaky =/
            val tq = "\"\"\""
            check.session(
              s"""
          @ import scalatags.Text.all._
          error: not found: value scalatags

          @ import $$ivy.`com.lihaoyi::scalatags:0.5.4`

          @ import scalatags.Text.all._
          import scalatags.Text.all._

          @ a("omg", href:="www.google.com").render
          res2: String = $tq
          <a href="www.google.com">omg</a>
          $tq
        """)
          }
        }
        'akkahttp - {
            check.session(
              """
              @ import $ivy.`com.typesafe.akka::akka-http-experimental:1.0-M3`

              @ implicit val system = akka.actor.ActorSystem()

              @ val akkaSystem = akka.http.Http(system)

              @ val serverBinding = akkaSystem.bind(interface = "localhost", port = 31337)

              @ implicit val materializer = akka.stream.ActorFlowMaterializer()

              @ var set = false

              @ serverBinding.connections.runForeach { connection =>
              @   set = true
              @ }

              @ set
              res7: Boolean = false

              @ akka.stream.scaladsl.Source(
              @   List(akka.http.model.HttpRequest(uri="/"))
              @ ).via(
              @   akka.http.Http().outgoingConnection("localhost", port=31337).flow
              @ ).runForeach(println)

              @ Thread.sleep(1000)

              @ set
              res10: Boolean = true

              @ system.shutdown()
             """)
        }
        'resolvers - {
          retry(2){
            // ivy flakyness...
            check.session("""
              @ import $ivy.`com.ambiata::mundane:1.2.1-20141230225616-50fc792`
              error: IvyResolutionException

              @ import ammonite._, Resolvers._

              @ val oss = Resolver.Http(
              @   "ambiata-oss",
              @   "https://ambiata-oss.s3-ap-southeast-2.amazonaws.com",
              @   IvyPattern,
              @   false
              @ )

              @ interp.resolvers() = interp.resolvers() :+ oss

              @ import $ivy.`com.ambiata::mundane:1.2.1-20141230225616-50fc792`

              @ import com.ambiata.mundane._
            """)
          }
        }
      }
      'code{
        check.session("""
          @ interp.load("val x = 1")

          @ x
          res2: Int = 1
        """)
      }
    }

    'shapeless {
      // Shapeless 2.1.0 isn't published for scala 2.10
      if (!scala2_10) check.session("""
        @ import $ivy.`com.chuusai::shapeless:2.2.5`, shapeless._

        @ (1 :: "lol" :: List(1, 2, 3) :: HNil)
        res1: Int :: String :: List[Int] :: HNil = 1 :: lol :: List(1, 2, 3) :: HNil

        @ res1(1)
        res2: String = "lol"

        @ import shapeless.syntax.singleton._

        @ 2.narrow
        res4: 2 = 2
      """)
    }

    'scalaz{
      check.session("""
        @ import $ivy.`org.scalaz::scalaz-core:7.1.1`, scalaz._, Scalaz._

        @ (Option(1) |@| Option(2))(_ + _)
        res1: Option[Int] = Some(3)
      """)
    }
    'guava{
      check.session("""
        @ import $ivy.`com.google.guava:guava:18.0`, com.google.common.collect._

        @ val bimap = ImmutableBiMap.of(1, "one", 2, "two", 3, "three")

        @ bimap.get(1)
        res2: String = "one"

        @ bimap.inverse.get("two")
        res3: Int = 2
      """)
    }
    'resources{
      check.session("""
        @ import ammonite.ops._

        @ val path = resource/'org/'apache/'jackrabbit/'oak/'plugins/'blob/"blobstore.properties"

        @ read! path
        error: ResourceNotFoundException

        @ import $ivy.`org.apache.jackrabbit:oak-core:1.3.16`

        @ read! path // Should work now
      """)
    }
    'scalaparse{
      // For some reason this blows up in 2.11.x
      // Prevent regressions when wildcard-importing things called `macro` or `_`
      if (!scala2_10) ()
      else
        check.session(s"""
          @ import scalaparse.Scala._

          @ 1
          res1: Int = 1

          @ ExprCtx.Parened.parse("1 + 1") // for some reason the tuple isn't pprinted
          res2: fastparse.core.Parsed[Unit,Char,String] = Failure("(":1:1 ..."1 + 1")

          @ ExprCtx.Parened.parse("(1 + 1)")
          res3: fastparse.core.Parsed[Unit,Char,String] = Success((), 7)
        """)
    }

    'finagle{
      // Prevent regressions when wildcard-importing things called `macro` or `_`
      check.session("""
        @ import $ivy.`com.twitter::finagle-httpx:6.26.0`

        @ import com.twitter.finagle._, com.twitter.util._

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
        res12: Int = 1

        @ clientResponse
        res13: Int = 200

        @ server.close()
      """)
    }
    'spire{
      // Prevent regressions when wildcard-importing things called `macro` or `_`
      if (!scala2_10) //buggy in 2.10
        check.session(s"""
          @ import $$ivy.`org.spire-math::spire:0.11.0`

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

          @ Interval(0, 10)
          res9: Interval[Int] = [0, 10]
        """)
      else
        check.session(s"""
          @ import $$ivy.`org.spire-math::spire:0.11.0`

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

          @ Interval(0, 10)
          res9: spire.math.Interval[Int] = [0, 10]
        """)

      // This fella is misbehaving but I can't figure out why :/
      //
      //          @ mean(Rational(1, 2), Rational(3, 2), Rational(0))
      //      res9: spire.math.Rational = 2/3

    }

  }
}

