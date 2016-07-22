import ammonite.ops._
val x = 1

@main
def noArgMain() = {
  println("Hello! " + x)
}

@main
def multiArgMain(i: Int, s: String, path: Path = cwd) = {
  println(s"Hello! ${s * i} ${path.relativeTo(cwd)}.")
}