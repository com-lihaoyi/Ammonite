package ammonite.repl.tools

object Util {

  /**
   * Additional [[mainargs.TokensReader]] instance to teach it how to read Ammonite paths
   */
  implicit object PathRead extends mainargs.TokensReader.Simple[os.Path] {
    def shortName = "path"
    def read(strs: Seq[String]) = Right(os.Path(strs.last, os.pwd))
  }

}
