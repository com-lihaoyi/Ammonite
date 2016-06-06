println("Begin spark test")

@
import ammonite.ops._
println("Loading dep set 1")
load.ivy("org.apache.hadoop" % "hadoop-common" % "2.5.1")
load.ivy("org.apache.hadoop" % "hadoop-auth" % "2.5.1")
load.ivy("commons-configuration" % "commons-configuration" % "1.9")
load.ivy("commons-collections" % "commons-collections" % "3.2.1")
load.ivy("commons-lang" % "commons-lang" % "2.6")
@
println("Loading dep set 2")
load.ivy("org.apache.spark" %% "spark-core" % "1.6.1")
load.ivy("org.apache.spark" %% "spark-repl" % "1.6.1")
load.ivy("org.apache.spark" %% "spark-network-common" % "1.6.1")
load.ivy("org.apache.spark" %% "spark-network-shuffle" % "1.6.1")
load.ivy("com.fasterxml.jackson.core" % "jackson-core" % "2.5.1")
load.ivy("com.fasterxml.jackson.core" % "jackson-annotations" % "2.3.1")
load.ivy("com.fasterxml.jackson.core" % "jackson-databind" % "2.3.1")
load.ivy("org.slf4j" % "slf4j-simple" % "1.7.12")
load.ivy("org.apache.avro" % "avro" % "1.7.1")
load.ivy("org.eclipse.jetty.aggregate" % "jetty-all-server" % "8.1.14.v20131031")
load.ivy("javax.servlet" % "servlet-api" % "2.5")
@
println("Importing modules")
import java.io.File
import java.net.InetAddress
@
println("Importing jetty")
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.bio.SocketConnector
import org.eclipse.jetty.server.handler.{DefaultHandler, HandlerList, ResourceHandler}
import org.eclipse.jetty.util.thread.QueuedThreadPool
@
println("Importing sparkcontext")
import org.apache.spark.{SparkConf, SparkContext}
@
println("Defining simple http server")
class SimpleHttpServer(root: String) {
  private var server: Server = null
  private var port = 0

  start()

  def start() {
    if (server != null) {
      throw new Exception("Server is already started")
    } else {
      val (actualServer, actualPort) = doStart()
      server = actualServer
      port = actualPort
    }
  }

  /**
    * Actually start the HTTP server and return the server and port.
    */
  private def doStart(): (Server, Int) = {
    val server = new Server()

    val connector = new SocketConnector
    connector.setMaxIdleTime(60 * 1000)
    connector.setSoLingerTime(-1)
    connector.setPort(0)
    server.addConnector(connector)

    val threadPool = new QueuedThreadPool
    threadPool.setDaemon(true)
    server.setThreadPool(threadPool)
    val resHandler = new ResourceHandler
    resHandler.setResourceBase(new File(root).getAbsolutePath)

    val handlerList = new HandlerList
    handlerList.setHandlers(Array(resHandler, new DefaultHandler))

    server.setHandler(handlerList)

    server.start()
    val actualPort = server.getConnectors()(0).getLocalPort

    (server, actualPort)
  }

  def stop() {
    if (server == null) {
      throw new Exception("Server is already stopped")
    } else {
      server.stop()
      server = null
    }
  }

  /**
    * Get the URI of this HTTP server (http://host:port or https://host:port)
    */
  def uri: String = {
    if (server == null) {
      throw new Exception("Server is not started")
    } else {
      val host = InetAddress.getLocalHost().getHostName()
      s"http://${host}:$port"
    }
  }
}
@
println("Starting spark test")
val server = new SimpleHttpServer(classOutputDir.toString)
val conf = new SparkConf()
  .setAppName("test")
  .setMaster("local[*]")
  .set("spark.repl.class.uri", server.uri)
  // To allow retries for this test.
  .set("spark.driver.allowMultipleContexts", "true")
val sc = new SparkContext(conf)
val result = sc.parallelize(1 to 5).map(_ * 10).collect
@
server.stop
println(s"Spark result: $result")
