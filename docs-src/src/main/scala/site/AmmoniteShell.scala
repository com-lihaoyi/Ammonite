package site

import scalatags.Text.all._
import ba.sake.hepek.implicits._

object AmmoniteShell extends templates.AmmoniteBlogPage {

  override def pageSettings = super.pageSettings.withTitle("Shell")

  def ammoniteShellSection = Section(
    "Ammonite Shell",
    div("test")
  )

}
