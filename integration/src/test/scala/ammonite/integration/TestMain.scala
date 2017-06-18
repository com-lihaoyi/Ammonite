package ammonite.integration
object TestMain {
  def main(args: Array[String]): Unit = {
    val hello = "Hello"
    // Break into debug REPL with
    ammonite.Main(
      predefCode = "println(\"Starting Debugging!\")"
    ).run(
      "hello" -> hello,
      "fooValue" -> foo()
    )
  }
  def foo() = 1
}
