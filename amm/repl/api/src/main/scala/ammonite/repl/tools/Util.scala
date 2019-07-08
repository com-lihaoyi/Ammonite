package ammonite.repl.tools

object Util {

  /**
    * Additional [[scopt.Read]] instance to teach it how to read Ammonite paths
    */
  implicit def pathScoptRead: scopt.Read[os.Path] = scopt.Read.stringRead.map(os.Path(_, os.pwd))

}
