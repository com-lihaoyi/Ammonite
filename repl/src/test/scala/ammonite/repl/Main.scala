package ammonite.repl
import ammonite.ops._

object Main{
  def main(args: Array[String]): Unit = {
    Repl.run(
      predef = "",
      ammoniteHome = cwd/'target/".ammonite",
      code = None,
      predefFile = None,
      file = None
    )
  }
}