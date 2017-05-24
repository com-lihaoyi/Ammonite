package ammonite.main

/**
  * Give Ammonite the ability to read (linux) system proxy environment variables
  * and convert them into java proxy properties. Which allows Ammonite to work
  * through proxy automatically, instead of setting `System.properties` manually.
  *
  * See issue 460.
  *
  * Parameter pattern:
  * https://docs.oracle.com/javase/7/docs/api/java/net/doc-files/net-properties.html
  *
  * Created by cuz on 17-5-21.
  */
private[ammonite] object ProxyFromEnv {
  private lazy val KeyPattern ="""([\w\d]+)_proxy""".r
  private lazy val UrlPattern ="""([\w\d]+://)?(.+@)?([\w\d\.]+):(\d+)/?""".r

  /**
    * Get current proxy environment variables.
    */
  private def getEnvs =
    sys.env.map { case (k, v) => (k.toLowerCase, v.toLowerCase) }
      .filterKeys(_.endsWith("proxy"))

  /**
    * Convert single proxy environment variable to corresponding system proxy properties.
    */
  private def envToProps(env: (String, String)): Map[String, String] = env match {
    case ("no_proxy", noProxySeq) =>
      val converted = noProxySeq.split(""",""").mkString("|")
      //https uses the same as http's. Ftp need not to be set here.
      Map("http.nonProxyHosts" -> converted)

    case (KeyPattern(proto), UrlPattern(_, cred, host, port)) =>
      val propHost = s"$proto.proxyHost" -> host
      val propPort = s"$proto.proxyPort" -> port
      val propCred = if (cred.isDefined) {
        val credPair = cred.dropRight(1).split(":")
        val propUser = s"$proto.proxyUser" -> credPair.head
        val propPassword = credPair.drop(1).map(s"$proto.proxyPassword" -> _)
        Seq(propUser) ++ propPassword
      } else Nil
      Seq(propHost, propPort) ++ propCred toMap
    case bad => Map.empty
  }


  /**
    * Set system proxy properties from environment variables.
    * Existing properties will not be overwritten.
    */
  def setPropProxyFromEnv(envs: Map[String, String] = this.getEnvs): Unit = {
    val sysProps = sys.props
    val proxyProps = envs.flatMap { env =>
      val props = envToProps(env)
      if (props.isEmpty) println(s"Warn: environment variable$env cannot be parsed.")
      props
    }.filter(p => !sysProps.exists(sp => sp._1 == p._1))
    sysProps ++= proxyProps
  }

  /**
    * helper implicit conversion: add isDefined method to String.
    */
  implicit private class StringIsDefined(s: String) {
    def isDefined: Boolean = s != null && s.length > 0
  }

}
