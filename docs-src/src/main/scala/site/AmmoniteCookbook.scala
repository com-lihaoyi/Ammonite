package site

import scalatags.Text.all._
import ba.sake.hepek.html.structure.blog.Section
import templates.AmmoniteBlogPage
import utils.Imports._

object AmmoniteCookbook extends AmmoniteBlogPage {

  override def pageTitle = "Cookbook"

  def ammoniteCookbookSection = Section(
    "Ammonite Cookbook",
    div("test")
  )

}
