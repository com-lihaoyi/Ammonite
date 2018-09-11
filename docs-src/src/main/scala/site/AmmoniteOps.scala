package site

import scalatags.Text.all._
import ba.sake.hepek.implicits._

object AmmoniteOps extends templates.AmmoniteBlogPage {

  override def pageSettings = super.pageSettings.withTitle("Ops")

  def ammoniteOpsSection = Section(
    "Ammonite Ops",
    div("test")
  )

}
