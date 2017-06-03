// Args.sc
val x = 1
import ammonite.ops._

@main
def main(i: Int, s: String, path: Path = pwd) = {
  s"Hello! ${s * i} ${path.last}."
}