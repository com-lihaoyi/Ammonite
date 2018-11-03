// MultiMain.sc

val x = 1

@main
def mainA() = {
  println("Hello! " + x)
}

@main
def functionB(i: Int, s: String, path: os.Path = os.pwd) = {
  println(s"Hello! ${s * i} ${path.relativeTo(os.pwd)}.")
}