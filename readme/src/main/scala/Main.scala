import ammonite.ops._

import scala.util.matching.Regex
import scalatags.Text.all._
import scalatex.site.Highlighter.RefPath
import ammonite.ops.cwd
object Main{
  def main(args: Array[String]): Unit = {
    val site = new scalatex.site.Site {
      def content = Map(
        "index.html" -> Readme()
      )
    }
    site.renderTo(cwd/'readme/'target/'output2)
  }

  object sect extends scalatex.site.Section
  object hl extends scalatex.site.Highlighter{
    def suffixMappings = Map(
      "scala" -> "scala",
      "sbt" -> "scala"
    )
    def scala(s: String) = this.highlight(s, "scala")
    override def pathMappings = Seq(
      cwd -> "https://github.com/lihaoyi/ammonite/tree/master"
    )
  }
  def late(frags: => Frag) = new Late(() => frags)
  class Late(frags: () => Frag) extends scalatags.text.Frag{
    def render: String = frags().render
    def writeTo(strb: StringBuilder): Unit = strb.append(render)
  }
  val fileOps = read! cwd/'ops/'src/'main/'scala/'ammonite/'ops/"FileOps.scala"
  val found = ".*/\\*\\*(\n\\s*\\*.*)+\n.*?extends.*?Op[^{]*".r
    .findAllIn(fileOps)
    .mkString("\n")
    .lines
    .map(" "+_)
    .mkString("\n")


}
