
import $ivy.`com.softwaremill.sttp.client::core:2.0.6`
import sttp.client.quick._
quickRequest.get(uri"http://httpbin.org/ip").send()
