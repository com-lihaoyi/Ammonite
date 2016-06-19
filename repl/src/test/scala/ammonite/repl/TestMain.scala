package ammonite.repl


object TestMain{
  def main(args: Array[String]): Unit = {
    Main.main(args ++ Array("--home", "target/tempAmmoniteHome"))
  }
}