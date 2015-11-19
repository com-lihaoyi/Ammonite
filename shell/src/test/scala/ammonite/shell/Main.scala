package ammonite.shell

/**
 * Convenience entry-point useful to kick off a shell with
 */
object Main {
  val examplePredef = "shell/src/main/resources/ammonite/shell/example-predef-bare.scala"
  def main(args: Array[String]) = {
    ammonite.repl.Repl.main(
      Array("--predef-file", examplePredef) ++ args
    )
  }
}
