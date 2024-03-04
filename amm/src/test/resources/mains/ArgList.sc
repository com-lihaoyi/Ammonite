@main
def main(args: String*): Unit = {
  args.foreach(arg => println(s"Argument: $arg"))
}
