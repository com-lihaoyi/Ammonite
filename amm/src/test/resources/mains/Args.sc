// Args.sc
val x = 1

@main
def main(i: Int, s: String, path: os.Path = os.pwd) = {
  s"Hello! ${s * i} ${path.last}."
}
