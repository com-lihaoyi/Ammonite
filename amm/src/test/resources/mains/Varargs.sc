// Varargs.sc
@main
def main(i: Int, s: String, things: String*): Unit = {
  println(i)
  println(s)
  println(things.toList)
}