package ammonite.integration

import ammonite.integration.TestUtils._
import utest._

object ProjectTests213 extends TestSuite{

  val tests = Tests {
    println("Running ProjectTest213")

    test("httpApi"){
      def addPostTest() = jsonplaceholder.withServer { url =>
        val res = execWithEnv(
          Seq("JSONPLACEHOLDER" -> url),
          os.rel / 'basic / "HttpApi.sc",
          "addPost", "title", "some text"
        )
        assert(res.out.trim.contains("101"))
      }
      def commentsTest() = jsonplaceholder.withServer { url =>
        val res = execWithEnv(
          Seq("JSONPLACEHOLDER" -> url),
          os.rel / 'basic / "HttpApi.sc",
          "comments", "40"
        )
        assert(res.out.trim.contains("totam vel saepe aut"))
        assert(res.out.trim.contains("aperiam et omnis totam"))
      }
      test("addPost") {
        if (isScala2) addPostTest()
        else "Disabled in Scala 3"
      }
      test("comments") {
        if (isScala2) commentsTest()
        else "Disabled in Scala 3"
      }
    }
  }
}

object jsonplaceholder extends cask.MainRoutes {

  private val comments = Map(
    40 -> ujson.read(
      os.read(replStandaloneResources / 'basic / "http-api-data.json")
    )
  )

  @cask.postForm("/posts")
  def posts(title: String, body: String, userId: String) = {
    System.err.println(s"got request $title, $body, $userId")
    cask.Response(ujson.Obj("id" -> 101).render())
  }

  @cask.get("/comments")
  def commentsHandler(postId: String) = {
    val postId0 = scala.util.Try(postId.toInt).getOrElse(0)
    val resp = comments.getOrElse(postId0, ujson.Obj())
    cask.Response(resp)
  }

  initialize()

  def withServer[T](f: String => T): T = {
    import scala.collection.JavaConverters._
    var server: io.undertow.Undertow = null
    try {
      server = io.undertow.Undertow.builder
        .addHttpListener(0, "localhost")
        .setHandler(defaultHandler)
        .build()
      server.start()
      val addr = server
        .getListenerInfo
        .asScala
        .head
        .getAddress
        .asInstanceOf[java.net.InetSocketAddress]
      val url = s"http://${addr.getHostString}:${addr.getPort}"
      f(url)
    } finally {
      if (server != null)
        server.stop()
    }
  }
}
