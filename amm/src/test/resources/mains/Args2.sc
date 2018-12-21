// Args.sc
val x = 1
@main
def main(i: Int, s: String, path: os.Path = os.pwd): Unit = {
  println(s"Hello! ${s * i} ${path.last}.")
}