package ammonite.tools

/**
 * Convenience entry-point useful to kick off a shell with
 */
object Main {
  val examplePredef = "tools/src/main/resources/example-predef.scala"
  def main(args: Array[String]) = {
    ammonite.repl.Repl.main(
      Array("--predef-file", examplePredef) ++ args
    )
  }
}
