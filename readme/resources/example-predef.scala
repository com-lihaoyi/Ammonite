import ammonite.ops._

prompt.bind(
  Console.RESET + sys.props("user.name") +
  Console.MAGENTA + "@" +
  Console.RESET + wd.segments.lastOption.getOrElse("").toString +
  Console.MAGENTA + ">\n"
)