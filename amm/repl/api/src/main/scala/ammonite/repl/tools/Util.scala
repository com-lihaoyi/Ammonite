package ammonite.repl.tools

object Util {

  /**
    * Additional [[mainargs.TokensReader]] instance to teach it how to read Ammonite paths
    */
  implicit object PathRead
  extends mainargs.TokensReader[os.Path]("path", strs => Right(os.Path(strs.last, os.pwd)))

}
