@main
def main(args: mainargs.Leftover[String]): Unit = {
  args.value.foreach(arg => println(s"Argument: $arg"))
}