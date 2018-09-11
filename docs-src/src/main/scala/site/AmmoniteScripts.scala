package site

import scalatags.Text.all._
import ba.sake.hepek.implicits._

object AmmoniteScripts extends templates.AmmoniteBlogPage {

  override def pageSettings = super.pageSettings.withTitle("Scripts")

  def ammoniteScriptsSection = Section(
    "Ammonite Scripts",
    div("test")
  )

}
