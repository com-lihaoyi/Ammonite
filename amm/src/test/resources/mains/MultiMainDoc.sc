// MultiMainDoc.sc

val x = 1

@main
def mainA() = {
  println("Hello! " + x)
}

@doc("This explains what the function does")
@main
def functionB(i: Int @doc(
                "how many times to repeat the string to make " +
                "it very very long, more than it originally was"
              ),
              s: String @doc("the string to repeat"),
              path: os.Path = os.pwd) = {
  println(s"Hello! ${s * i} ${path.relativeTo(os.pwd)}.")
}