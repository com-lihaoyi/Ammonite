package ammonite.tools

/**
 * Convenience entry-point useful to kick off a shell with
 */
object Main {
  def main(args: Array[String]) = {
    ammonite.repl.Repl.main(
      Array("--predef-file", "readme/resources/example-predef.scala") ++ args
    )
  }
}
