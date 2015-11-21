package ammonite.integration

object Main {
  def main(args: Array[String]): Unit = {
    import ammonite.ops._
    ammonite.repl.Repl.run(
      predef = "import ammonite.integration.Main._",
      predefFile = Some(
        cwd/'shell/'src/'main/'resources/'ammonite/'shell/"example-predef-bare.scala"
      )
    )
  }
  def foo() = 1
}
