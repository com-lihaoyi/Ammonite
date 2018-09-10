package site

import scalatags.Text.all._
import ba.sake.hepek.implicits._

object AmmoniteCookbook extends templates.AmmoniteBlogPage {

  override def pageSettings = super.pageSettings.withTitle("Cookbook")

  def ammoniteCookbookSection = Section(
    "Ammonite Cookbook",
    div("test")
  )

}
