package ammonite.repl
import ammonite.ops._
import ammonite.repl.Util.CompileCache

object TestMain{
  def main(args: Array[String]): Unit = {
    System.setProperty("ammonite-sbt-build", "true")
    Main(
      defaultPredef = false,
      storageBackend = new Storage.Folder(cwd/'target/'tempAmmoniteHome)
    ).run()
  }
}