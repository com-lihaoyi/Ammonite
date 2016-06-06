package ammonite.repl


object TestMain{
  def main(args: Array[String]): Unit = {
    Main(
      // Any arguments to configure the REPL here, e.g. changing the home
      // folder from the default of ~/.ammonite to some local folder to avoid
      // interference with other Ammonite REPLs running on your system
      storageBackend =
        new Storage.Folder(ammonite.ops.cwd/'target/'tempAmmoniteHome)
    ).run()
  }
}