package ammonite

object TestMain{
  def main(args: Array[String]): Unit = {
    System.setProperty("ammonite-sbt-build", "true")

    Main.main(Array("--home", "target/tempAmmoniteHome") ++ args)

  }
}