package site

import scalatags.Text.all._
import ba.sake.hepek.html.structure.blog.Section
import templates.AmmoniteBlogPage
import utils.Imports._

object AmmoniteOps extends AmmoniteBlogPage {

  override def pageTitle = "Ops"

  def ammoniteOpsSection = Section(
    "Ammonite Ops",
    div("test")
  )

}
