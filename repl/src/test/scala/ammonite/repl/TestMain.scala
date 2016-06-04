package ammonite.repl
import ammonite.ops._
import ammonite.repl.Util.CompileCache

object TestMain{
  def main(args: Array[String]): Unit = {
    System.setProperty("ammonite-sbt-build", "true")
    Main(
      storageBackend = new Storage.Folder(cwd/'target/'tempAmmoniteHome){
        override def compileCacheSave(tag: String, data: CompileCache) = ()
        override def compileCacheLoad(tag: String) = None
      }
    ).run()
  }
}