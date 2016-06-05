package ammonite.repl
import ammonite.ops._
import ammonite.repl.Util.CompileCache

object TestMain{
  def main(args: Array[String]): Unit = {
    System.setProperty("ammonite-sbt-build", "true")
    Main(
      storageBackend = new Storage.Folder(cwd/'target/'tempAmmoniteHome){
        override def compileCacheSave(path: String, tag: String, data: CompileCache) = ()
        override def compileCacheLoad(path: String, tag: String) = None
      }
    ).run()
  }
}