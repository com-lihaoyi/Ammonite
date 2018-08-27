package site

import scalatags.Text.all._
import ba.sake.hepek.html.structure.blog.Section
import templates.AmmoniteBlogPage
import utils.Imports._

object AmmoniteShell extends AmmoniteBlogPage {

  override def pageTitle = "Shell"

  def ammoniteShellSection = Section(
    "Ammonite Shell",
    div("test")
  )

}
