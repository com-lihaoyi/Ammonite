package ammonite.repl


object TestMain{
  def main(args: Array[String]): Unit = {
    Main.main(Array("--home", "target/tempAmmoniteHome") ++ args)
  }
}