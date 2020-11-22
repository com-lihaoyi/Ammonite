// Varargs.sc
@main
def main(i: Int, s: String, things: mainargs.Leftover[String]): Unit = {
  println(i)
  println(s)
  println(things.value.toList)
}