// Args.scala
val x = 1
cy = t
import ammonite.ops._
def main(i: Int, s: String, path: Path = cwd) = {
  println(s"Hello! ${s * i} ${path.relativeTo(cwd)}.")
}