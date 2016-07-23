// MultiMain.sc

import ammonite.ops._
val x = 1

@main
def mainA() = {
  println("Hello! " + x)
}

@main
def functionB(i: Int, s: String, path: Path = cwd) = {
  println(s"Hello! ${s * i} ${path.relativeTo(cwd)}.")
}