package site

import scalatags.Text.all._
import ba.sake.hepek.html.structure.blog.Section
import templates.AmmoniteBlogPage
import utils.Imports._

object AmmoniteScripts extends AmmoniteBlogPage {

  override def pageTitle = "Scripts"

  def ammoniteScriptsSection = Section(
    "Ammonite Scripts",
    div("test")
  )

}
