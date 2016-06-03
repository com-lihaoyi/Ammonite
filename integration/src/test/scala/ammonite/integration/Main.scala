package ammonite.integration

import ammonite.repl.Storage

object Main {
  def main(args: Array[String]): Unit = {
    import ammonite.ops._
    ammonite.repl.Main(
      predef = "import ammonite.integration.Main._",
      storageBackend = new Storage.Folder(cwd/'target/'tempAmmoniteHome){
        override val predef = {
          cwd/'shell/'src/'main/'resources/'ammonite/'shell/"example-predef-bare.scala"
        }
      }
    ).run()
  }
  def foo() = 1
  def bar() = "two"
}
