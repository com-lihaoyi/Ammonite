import $ivy.{
  `org.apache.hadoop:hadoop-common:2.5.1`,
  `org.apache.hadoop:hadoop-auth:2.5.1`,
  `commons-configuration:commons-configuration:1.9`,
  `commons-collections:commons-collections:3.2.1`,
  `commons-lang:commons-lang:2.6`,
  `org.apache.spark::spark-core:2.1.1`,
  `org.apache.spark::spark-repl:2.1.1`,
  `org.apache.spark::spark-network-common:2.1.1`,
  `org.apache.spark::spark-network-shuffle:2.1.1`,
  `org.eclipse.jetty.aggregate:jetty-all-server:8.1.14.v20131031`
}

import java.net.InetAddress
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.bio.SocketConnector
import org.eclipse.jetty.server.handler.{DefaultHandler, HandlerList, ResourceHandler}
import org.eclipse.jetty.util.thread.QueuedThreadPool
import org.eclipse.jetty.util.resource.Resource
import org.apache.spark.{SparkConf, SparkContext}

val server = {
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
  resHandler.setBaseResource(Resource.newClassPathResource("/"))

  val handlerList = new HandlerList
  handlerList.setHandlers(Array(resHandler, new DefaultHandler))

  server.setHandler(handlerList)

  server.start()

  server
}

val port = server.getConnectors()(0).getLocalPort

println("Starting spark test")

val conf = new SparkConf()
  .setAppName("test")
  .setMaster("local[*]")
  .set(
    "spark.repl.class.uri",
    s"http://${InetAddress.getLocalHost().getHostName()}:$port"
  )
  // To allow retries for this test.
  .set("spark.driver.allowMultipleContexts", "true")
val sc = new SparkContext(conf)
val result = sc.parallelize(1 to 5).map(_ * 10).collect

server.stop()
println(s"Spark result: ${result.toList}")
assert(result.toList == List(10, 20, 30, 40, 50))
