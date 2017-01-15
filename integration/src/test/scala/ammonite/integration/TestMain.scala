package ammonite.integration
object TestMain {
  def main(args: Array[String]): Unit = {
    val hello = "Hello"
    // Break into debug REPL with
    ammonite.Main(
      predef = "println(\"Starting Debugging!\")"
    ).run(Nil,
      "hello" -> hello,
      "fooValue" -> foo()
    )
  }
  def foo() = 1
}
