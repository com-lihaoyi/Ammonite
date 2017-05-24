package ammonite.main

import utest._
/**
  * This test is focused on function `setPropProxyFromEnv`, and not to test if proxy is OK or not.
  *
  * Created by cuzfrog on 1/31/17.
  */
object ProxyFromEnvTest {
  sys.props --= getProxyProps.keys
  //clear proxy properties
  private def getProxyProps: Map[String, String] = {
    //proper use of filterKeys, because in test and not heavy
    sys.props.filterKeys(_.contains("proxy")).toMap
  }

  val positives: Map[String, String] = Map(
    "https_proxy" -> "https://user1:pass3@localhost:808",
    "socks5_proxy" -> "socks5://user2@192.168.1.6:808/",
    "ftp_proxy" -> "127.0.0.1:1020"
  )
  val nagetives: Map[String, String] = Map(
    "bad#name_proxy" -> "badname://user1:pass3@192.168.1.8:808",
    "badHost_proxy" -> "https://user1:pass3@###:808",
    "badPort_proxy" -> "https://localhost:d",
    "empty_proxy" -> "",
    "empty2_proxy" -> null
  )

  import ProxyFromEnv._

//  val tests = this {
//    "print-test" - {
//      println("=====before setting proxy===")
//      getProxyProps.foreach(println)
//      println("=====there should be noting above")
//      println("=====after setting proxy")
//      setPropProxyFromEnv(positives)
//      getProxyProps.foreach(println)
//      println(s"=====should print at least ${nagetives.size} warings.")
//      setPropProxyFromEnv(nagetives)
//    }
//    "conflict-test" - {
//      System.setProperty("https.proxyHost", "10.0.0.101")
//      System.setProperty("https.proxyPort", "6000")
//      setPropProxyFromEnv(positives)
//      assert(getProxyProps("https.proxyHost") == "10.0.0.101")
//      assert(getProxyProps("https.proxyPort") == "6000")
//    }
//  }


}
