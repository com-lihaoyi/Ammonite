package ammonite.session

import ammonite.DualTestRepl
import ammonite.TestUtils._
import utest._

object ProjectTests extends TestSuite{
  val tests = Tests{
    println("ProjectTests")
    val check = new DualTestRepl()
    test("load"){
      test("ivy"){
        test("standalone"){
            // ivy or maven central are flaky =/
            val tq = "\"\"\""
            check.session(
              s"""
          @ import scalatags.Text.all._
          error: ${check.notFound("scalatags")}

          @ import $$ivy.`com.lihaoyi::scalatags:0.7.0 compat`

          @ import scalatags.Text.all._
          import scalatags.Text.all._

          @ a("omg", href:="www.google.com").render
          res2: String = "<a href=\\"www.google.com\\">omg</a>"
        """)
        }
        test("akkahttp"){
            if (scala2_11) check.session(
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
        test("resolvers"){
          retry(2){
            // ivy flakyness...
            check.session("""
              @ import $ivy.`com.lightbend::emoji:1.2.1`
              error: Failed to resolve ivy dependencies

              @ interp.repositories() ++= Seq(coursierapi.IvyRepository.of(
              @   "https://repo.typesafe.com/typesafe/ivy-releases/[defaultPattern]"
              @ ))

              @ import $ivy.`com.lightbend::emoji:1.2.1`

              @ import com.lightbend.emoji._
            """)
          }
        }
        test("resolversStatic"){
          check.session("""
            @ import $repo.`https://jitpack.io`

            @ import $ivy.`com.github.jupyter:jvm-repr:0.4.0`

            @ import jupyter._
          """)
        }
      }
      test("code"){
        check.session("""
          @ repl.load("val x = 1")

          @ x
          res2: Int = 1
        """)
      }
    }

    test("shapeless"){
      check.session("""
        @ import $ivy.`com.chuusai::shapeless:2.3.3`, shapeless._

        @ (1 :: "lol" :: List(1, 2, 3) :: HNil)
        res1: Int :: String :: List[Int] :: HNil = 1 :: "lol" :: List(1, 2, 3) :: HNil

        @ res1(1)
        res2: String = "lol"

        @ import shapeless.syntax.singleton._

        @ 2.narrow
        res4: 2 = 2
      """)
    }

    test("scalaz"){
      check.session(s"""
        @ import $$ivy.`org.scalaz::scalaz-core:7.2.27 compat`, scalaz._, Scalaz._

        @ (Option(1) |@| Option(2))(_ + _)
        res1: Option[Int] = ${Print.Some(value = 3)}
      """)
    }
    test("cats"){
      check.session("""
        @ import $ivy.`org.typelevel::cats-core:2.0.0-M4 compat`, cats._

      """)
    }
    test("guava"){
      check.session("""
        @ import $ivy.`com.google.guava:guava:18.0`, com.google.common.collect._

        @ val bimap = ImmutableBiMap.of(1, "one", 2, "two", 3, "three")

        @ bimap.get(1)
        res2: String = "one"

        @ bimap.inverse.get("two")
        res3: Int = 2
      """)
    }
    test("resources"){
      if (!scala2_12) check.session("""
        @ import ammonite.ops._

        @ val path = {
        @   resource/"org"/"apache"/"jackrabbit"/"oak"/"plugins"/"blob"/"blobstore.properties"
        @ }

        @ read! path
        error: ResourceNotFoundException

        @ import $ivy.`org.apache.jackrabbit:oak-core:1.3.16`

        @ read! path // Should work now
      """)
    }
    test("scalaparse"){
      // For some reason this blows up in 2.11.x
      // Prevent regressions when wildcard-importing things called `macro` or `_`
      if (scala2_12) check.session(s"""
        @ import $$ivy.`com.lihaoyi::scalaparse:2.0.5`, scalaparse.Scala._

        @ 1
        res1: Int = 1

        @ fastparse.parse("1 + 1", ExprCtx.Parened(_)).toString
        res2: String = "Parsed.Failure(Position 1:1, found \\"1 + 1\\")"

        @ fastparse.parse("(1 + 1)", ExprCtx.Parened(_)).toString
        res3: String = "Parsed.Success((), 7)"
      """)
    }

    test("finagle"){
      // Prevent regressions when wildcard-importing things called `macro` or `_`
      check.session("""
        @ import $ivy.`com.twitter::finagle-http:21.2.0`

        @ import com.twitter.finagle._, com.twitter.util._

        @ var serverCount = 0

        @ var clientResponse = 0

        @ val service = new Service[http.Request, http.Response] {
        @   def apply(req: http.Request): Future[http.Response] = {
        @     serverCount += 1
        @     Future.value(
        @       http.Response(req.version, http.Status.Ok)
        @     )
        @   }
        @ }

        @ val server = Http.serve(":8080", service)

        @ val client: Service[http.Request, http.Response] = Http.client.newService(":8080")

        @ val request = http.Request(http.Method.Get, "/")

        @ request.host = "www.scala-lang.org"

        @ val response: Future[http.Response] = client(request)

        @ response.onSuccess { resp: http.Response =>
        @   clientResponse = resp.statusCode
        @ }

        @ Await.ready(response)

        @ serverCount
        res12: Int = 1

        @ clientResponse
        res13: Int = 200

        @ server.close()
      """)
    }
    test("spire"){
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
    test("pegdown"){
      check.session(
        s"""
           @ import $$ivy.`org.pegdown:pegdown:1.6.0`

           @ org.pegdown.ast.SimpleNode.Type.HRule
           res1: org.pegdown.ast.SimpleNode.Type = HRule
         """)
    }

    test("deeplearning"){
      // DeepLearning.scala 2.0.0-RC0 isn't published for scala 2.13
      if (scala2_12) check.session(
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

    test("jadb"){
      // tests for jitpack and optional dependencies
      check.session(
        """
        @ interp.repositories() ++= Seq(
        @     coursierapi.MavenRepository.of("https://jitpack.io")
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

        @ import $ivy.`com.chuusai::shapeless:2.3.3 compat`
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

    test("profiles"){
      val testCore =
        """
            @ import $ivy.`org.apache.spark::spark-sql:2.4.3`

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
      test("default"){
        // should load hadoop 2.6 stuff by default
        if (scala2_12)
          check.session(
            s"""
            $testCore

            @ val hadoopVersion = p.getProperty("version")
            hadoopVersion: String = "2.6.5"
            """
          )
      }
      test("withProfile"){
        // with the right profile, should load hadoop 3.1 stuff
        if (scala2_12)
          check.session(
            s"""
            @ interp.resolutionHooks += { fetch =>
            @   fetch.withResolutionParams(
            @     fetch
            @       .getResolutionParams
            @       .addProfile("hadoop-3.1")
            @   )
            @ }

            $testCore

            @ val hadoopVersion = p.getProperty("version")
            hadoopVersion: String = "3.1.0"
            """
          )
      }
    }
    test("onlyJarLikeArtifactTypes"){
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

    test("no sources"){
      val sbv = scala.util.Properties.versionNumberString.split('.').take(2).mkString(".")

      val core =
        s"""
            @ import $$ivy.`com.github.alexarchambault::case-app:2.0.0-M9 compat`

            @ val cp = {
            @   repl.sess
            @     .frames
            @     .flatMap(_.classpath)
            @     .map(_.toString)
            @ }
            cp: List[String] = ?

            @ val found = cp.exists(_.endsWith("/case-app_$sbv-2.0.0-M9.jar"))
            found: Boolean = true
        """

      test("default"){
        check.session(
          s"""
            $core

            @ val sourcesFound = cp.exists(_.endsWith("/case-app_$sbv-2.0.0-M9-sources.jar"))
            sourcesFound: Boolean = true
           """
        )
      }

      test("disabled"){
        check.session(
          s"""
            @ interp.resolutionHooks += { fetch =>
            @   import scala.collection.JavaConverters._
            @   fetch.withClassifiers(fetch.getClassifiers.asScala.filter(_ != "sources").asJava)
            @ }

            $core

            @ val sourcesFound = cp.exists(_.endsWith("/case-app_$sbv-2.0.0-M9-sources.jar"))
            sourcesFound: Boolean = false
           """
        )
      }
    }

    test("extra artifact types"){
      val core =
        """
            @ import $ivy.`com.almworks.sqlite4java:libsqlite4java-linux-amd64:1.0.392`

            @ val cp = {
            @   repl.sess
            @     .frames
            @     .flatMap(_.classpath)
            @     .map(_.toString)
            @ }
            cp: List[String] = ?

            @ def soFound() = cp.exists(_.endsWith("/libsqlite4java-linux-amd64-1.0.392.so"))
            defined function soFound
        """

      test("default"){
        check.session(
          s"""
            $core

            @ val soFound0 = soFound()
            soFound0: Boolean = false
           """
        )
      }

      test("with so"){
        check.session(
          s"""
            @ interp.resolutionHooks += { fetch =>
            @   fetch.addArtifactTypes("so")
            @ }

            $core

            @ val soFound0 = soFound()
            soFound0: Boolean = true
           """
        )
      }
    }

  }
}

