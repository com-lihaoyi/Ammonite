package ammonite.session

import ammonite.DualTestRepl
import ammonite.TestUtils._
import utest._

import scala.collection.{immutable => imm}

object ProjectTests extends TestSuite{
  val tests = Tests{
    println("ProjectTests")
    val check = new DualTestRepl()
    'load {
      'ivy {
        'standalone - {
            // ivy or maven central are flaky =/
            val tq = "\"\"\""
            check.session(
              s"""
          @ import scalatags.Text.all._
          error: not found: value scalatags

          @ import $$ivy.`com.lihaoyi::scalatags:0.6.2`

          @ import scalatags.Text.all._
          import scalatags.Text.all._

          @ a("omg", href:="www.google.com").render
          res2: String = "<a href=\\"www.google.com\\">omg</a>"
        """)
        }
        'akkahttp - {
            if (!scala2_12) check.session(
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
            if (!scala2_12) check.session("""
              @ import $ivy.`com.ambiata::mundane:1.2.1-20141230225616-50fc792`
              error: Failed to resolve ivy dependencies

              @ interp.repositories() ++= Seq(coursier.ivy.IvyRepository.fromPattern(
              @   "https://ambiata-oss.s3-ap-southeast-2.amazonaws.com/" +:
              @   coursier.ivy.Pattern.default
              @ ))

              @ import $ivy.`com.ambiata::mundane:1.2.1-20141230225616-50fc792`

              @ import com.ambiata.mundane._
            """)
          }
        }
      }
      'code{
        check.session("""
          @ repl.load("val x = 1")

          @ x
          res2: Int = 1
        """)
      }
    }

    'shapeless {
      check.session("""
        @ import $ivy.`com.chuusai::shapeless:2.3.2`, shapeless._

        @ (1 :: "lol" :: List(1, 2, 3) :: HNil)
        res1: Int :: String :: List[Int] :: HNil = 1 :: "lol" :: List(1, 2, 3) :: HNil

        @ res1(1)
        res2: String = "lol"

        @ import shapeless.syntax.singleton._

        @ 2.narrow
        res4: 2 = 2
      """)
    }

    'scalaz{
      check.session("""
        @ import $ivy.`org.scalaz::scalaz-core:7.2.7`, scalaz._, Scalaz._

        @ (Option(1) |@| Option(2))(_ + _)
        res1: Option[Int] = Some(3)
      """)
    }
    'cats{
      check.session("""
        @ import $ivy.`org.typelevel::cats-core:0.9.0`, cats._

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
      if (!scala2_12) check.session("""
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
      if (scala2_12) check.session(s"""
        @ import scalaparse.Scala._

        @ 1
        res1: Int = 1

        @ fastparse.parse("1 + 1", ExprCtx.Parened(_)).toString
        res2: String = "Parsed.Failure(Position 1:1, found \\"1 + 1\\")"

        @ fastparse.parse("(1 + 1)", ExprCtx.Parened(_)).toString
        res3: String = "Parsed.Success((), 7)"
      """)
    }

    'finagle{
      // Prevent regressions when wildcard-importing things called `macro` or `_`
      if (!scala2_12) check.session("""
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
      //buggy in 2.10, spire not yet published for 2.12
      if (scala2_11)
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
          res9: Interval[Int] = Bounded(0, 10, 0)

          @ mean(Rational(1, 2), Rational(3, 2), Rational(0))
          res10: Rational = 2/3
        """)
          }
    'pegdown{
      check.session(
        s"""
           @ import $$ivy.`org.pegdown:pegdown:1.6.0`

           @ org.pegdown.ast.SimpleNode.Type.HRule
           res1: org.pegdown.ast.SimpleNode.Type = HRule
         """)
    }

    'deeplearning {
      // DeepLearning.scala 2.0.0-RC0 isn't published for scala 2.10
      check.session(
        """
        @ import $ivy.`com.thoughtworks.deeplearning::plugins-builtins:2.0.0-RC0`
        import $ivy.$

        @ import $ivy.`org.nd4j:nd4j-native-platform:0.8.0`
        import $ivy.$

        @ import scala.concurrent.ExecutionContext.Implicits.global
        import scala.concurrent.ExecutionContext.Implicits.global

        @ import com.thoughtworks.feature.Factory
        import com.thoughtworks.feature.Factory

        @ import com.thoughtworks.deeplearning._
        import com.thoughtworks.deeplearning._

        @ Factory[plugins.Builtins].newInstance()
        """
      )
    }

    'jadb {
      // tests for jitpack and optional dependencies
      check.session(
        """
        @ interp.repositories() ++= Seq(
        @     coursier.maven.MavenRepository("https://jitpack.io")
        @ )

        @ import $ivy.`com.github.vidstige:jadb:v1.0.1`
        import $ivy.$

        @ import se.vidstige.jadb.JadbConnection
        import se.vidstige.jadb.JadbConnection
        """
      )
    }

    "no duplicate Ammonite JARs" - {
      check.session(
        """
        @ def scalaLibJarCount() = {
        @   import scala.collection.JavaConverters._
        @   Thread.currentThread
        @     .getContextClassLoader
        @     .getResources("library.properties")
        @     .asScala
        @     .length
        @ }
        defined function scalaLibJarCount

        @ val initCount = scalaLibJarCount()

        @ // should only add the shapeless-specific JARs

        @ import $ivy.`com.chuusai::shapeless:2.3.2`
        import $ivy.$

        @ val addedAfterShapeless = scalaLibJarCount() - initCount
        addedAfterShapeless: Int = 0

        @ // shouldn't add any JAR (scala-library already pulled by Ammonite)

        @ import $ivy.`org.scala-lang:scala-library:2.11.12`
        import $ivy.$

        @ val addedAfterScalaLib = scalaLibJarCount() - initCount
        addedAfterScalaLib: Int = 0
        """
      )
    }

    'profiles {
      val testCore =
        """
            @ import $ivy.`org.apache.spark::spark-sql:1.6.2`

            @ import scala.collection.JavaConverters._

            @ val p = new java.util.Properties
            p: java.util.Properties = {}

            @ p.load(
            @   Thread.currentThread()
            @     .getContextClassLoader
            @     .getResources("common-version-info.properties")
            @     .asScala
            @     .toVector
            @     .find(!_.toString.contains("-sources.jar"))
            @     .getOrElse(sys.error("common-version-info.properties not found in classpath"))
            @     .openStream()
            @ )
        """
      'default {
        // should load hadoop 2.2 stuff by default
        if (scala2_11)
          check.session(
            s"""
            $testCore

            @ val hadoopVersion = p.getProperty("version")
            hadoopVersion: String = "2.2.0"
            """
          )
      }
      'withProfile {
        // with the right profile, should load hadoop 2.6 stuff
        if (scala2_11)
          check.session(
            s"""
            @ interp.resolutionHooks += { fetch =>
            @   fetch.withResolutionParams(
            @     // With coursier > 1.1.0-M13-1, replace with
            @     //   fetch.resolutionParams
            @     //     .addProfile("hadoop-2.6")
            @     coursier.params.ResolutionParams()
            @       .withProfiles(Set("hadoop-2.6"))
            @   )
            @ }

            $testCore

            @ val hadoopVersion = p.getProperty("version")
            hadoopVersion: String = "2.6.0"
            """
          )
      }
    }
    'onlyJarLikeArtifactTypes {
      check.session(
        """
           @ import $ivy.`log4j:log4j:1.2.17`

           @ val log4jStuff = {
           @   repl
           @     .sess
           @     .frames
           @     .flatMap(_.classpath)
           @     .filter(_.getPath.split('/').last.contains("log4j"))
           @ }

           @ assert(
           @   // no zip or tar.gz stuff in particular
           @   log4jStuff.forall(_.getPath.split('/').last.endsWith(".jar"))
           @ )
           """
      )
    }

  }
}

