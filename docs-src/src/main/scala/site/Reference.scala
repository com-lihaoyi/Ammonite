package site

import scalatags.Text.all._
import ba.sake.hepek.html.structure.blog.Section
import templates.AmmoniteBlogPage
import utils.Imports._

object Reference extends AmmoniteBlogPage {

  override def pageTitle = "Reference"

  def inTheWildSection = Section(
    "inTheWildSection",
    div("inTheWildSection")
  )

  def talksSection = Section(
    "talksSection",
    div("talksSection")
  )

  def unstableVersionsSection = Section(
    "unstableVersionsSection",
    div("unstableVersionsSection")
  )

}
