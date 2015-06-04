package ammonite.pprint
import acyclic.file

/**
 * Configuration options to control how prettyprinting occurs, passed
 * recursively into each prettyprinting callsite.
 *
 * @param maxWidth Controls how far to the right a line will go before
 *                 it tries to wrap
 * @param lines Controls how many lines can be printed at once.
 *                  Will print all lines if set to 0
 * @param depth How much the current item being printed should be indented
 * @param renames A map used to rename things to more common names, e.g.
 *                renamig `WrappedArray` to `Array` or getting rid of
 *                TupleN *
 */
case class Config(maxWidth: () => Int = () => 100,
                  lines: () => Int = () => 0,
                  depth: Int = 0,
                  indent: Int = 2,
                  literalColor: String = null,
                  prefixColor: String = null,
                  renames: Map[String, String] = Config.defaultRenames)
  extends GenConfig[Config]{
  def deeper = copy(depth = depth + 1)

  def rename(s: String) = renames.getOrElse(s, s)
  object color{
    def apply(s: String, c: String) = {
      if (c == null) s
      else c + s + Console.RESET
    }
    def literal(s: String) = apply(s, literalColor)
    def prefix(s: String) = apply(s, prefixColor)
  }
}

object Config {
  val defaultRenames = Map(
    "WrappedArray" -> "Array"
  ) ++ (2 to 22).map(i => s"Tuple$i" -> "")

  object Defaults {
    implicit val PPrintConfig = Config()
  }
  object Colors {
    implicit val PPrintConfig = Config(
      literalColor = Console.GREEN,
      prefixColor = Console.YELLOW
    )
  }
}

/**
 * Helpers to help inject behavior into the generated code
 * without having any circular dependencies
 */
trait GenConfig[T <: GenConfig[T]]{
  def deeper: T
  def rename(s: String): String
}

/**
 * Helpers to help inject behavior into the generated code
 * without having any circular dependencies
 */
trait GenUtils{
  type PP[T]
  type C <: GenConfig[C]
  def render[T: PP](t: T, c: C): Iterator[String]
}
