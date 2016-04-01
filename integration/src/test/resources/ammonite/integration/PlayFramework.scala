/**
  * Single-file play framework application! Make sure everything
  * works, as this is the test case that un-earthed #371
  */
load.ivy("com.typesafe.play" %% "play" % "2.5.0")
load.ivy("com.typesafe.play" %% "play-netty-server" % "2.5.0")
load.ivy("org.scalaj" %% "scalaj-http" % "2.2.1")

@

import play.core.server._, play.api.routing.sird._, play.api.mvc._
import scalaj.http._
val server = NettyServer.fromRouter(new ServerConfig(
  rootDir = new java.io.File("."),
  port = Some(19000), sslPort = None,
  address = "0.0.0.0", mode = play.api.Mode.Dev,
  properties = System.getProperties,
  configuration = play.api.Configuration(
    "play.server.netty" -> Map(
      "maxInitialLineLength" -> 4096,
      "maxHeaderSize" -> 8192,
      "maxChunkSize" -> 8192,
      "log.wire" -> false,
      "eventLoopThreads" -> 0,
      "transport" -> "jdk",
      "option.child" -> Map()
    )
  )
)) {
  case GET(p"/hello/$to") => Action { Results.Ok(s"Hello $to") }
}
try {
  println(Http("http://localhost:19000/hello/bar").asString.body)
}finally{
  server.stop()
}
