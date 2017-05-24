package ammonite

object TestMain{
  def main(args: Array[String]): Unit = {
    System.setProperty("ammonite-sbt-build", "true")
    val homeFlag = Array("--home", "target/tempAmmoniteHome")
    args match{
      case Array(first, rest@_*) if first.startsWith("--") => Main.main(args ++ homeFlag)
      case Array(first, rest@_*) => Main.main(Array(first) ++ homeFlag ++ Array("--") ++ rest)
      case _ => Main.main(homeFlag ++ args)
    }
  }
}