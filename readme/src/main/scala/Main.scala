import ammonite.ops._

import scala.util.matching.Regex
import scalatags.Text.all._
import scalatex.site.Highlighter.RefPath
import ammonite.ops.cwd
object Main{
  def main(args: Array[String]): Unit = {
    val ghLink = a(
      href:="https://github.com/lihaoyi/ammonite",
      position.absolute,
      top:=0,right:=0,border:=0,
      img(
        src:="https://camo.githubusercontent.com/a6677b08c955af8400f44c6298f40e7d19cc5b2d/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f677261795f3664366436642e706e67",
        alt:="Fork me on GitHub"
      )
    )

    val site = new scalatex.site.Site {

      override def autoResources = super.autoResources ++ Seq(
        root/'scalatex/'scrollspy/"scrollspy.js"
      )

      def content = Map(
        "index.html" -> Readme()
      )

      override def bodyFrag(frag: Frag) = {
        Seq(
          ghLink,
          div(
            position.fixed,
            overflow.scroll,
            backgroundColor := "#191818",
            height := "100%",
            width := 250,
            left := 0,
            top := 0,
            a(href:="#menu", id:="menu-link", cls:="menu-link")(
              span
            ),
            div(id:="menu")
          ),
          div(
            marginLeft := 250,
            super.bodyFrag(frag)
          ),
          script(raw(s"""
            scalatex.scrollspy.Controller().main(
              ${upickle.write(sect.structure.children(0))},
              document.getElementById("menu"),
              document.getElementById("menu-link")
          )"""))
        )
      }
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
