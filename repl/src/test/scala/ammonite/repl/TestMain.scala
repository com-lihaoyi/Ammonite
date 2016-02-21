package ammonite.repl
import ammonite.ops._

object TestMain{
  def main(args: Array[String]): Unit = {
    System.setProperty("ammonite-sbt-build", "true")
    Main.run(
      predef = "",
      ammoniteHome = cwd/'target/".ammonite",
      code = None,
      predefFile = None,
      file = None
    )
  }
}