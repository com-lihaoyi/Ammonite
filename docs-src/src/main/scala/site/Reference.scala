package site

import scalatags.Text.all._
import ba.sake.hepek.html.structure.blog.Section

object Reference extends templates.AmmoniteBlogPage {

  override def pageSettings = super.pageSettings.withTitle("Reference")

  override def blogSettings =
    super.blogSettings.withSections(referenceSection)

  def referenceSection = Section(
    "Reference",
    div("Reference"),
    List(communitySection,
         olderScalaVersionsSection,
         talksSection,
         inTheWildSection,
         scaladocSection,
         changelogSection,
         unstableVersionsSection)
  )

  def communitySection = Section(
    "Community",
    div("Community")
  )

  def olderScalaVersionsSection = Section(
    "Older Scala Versions",
    div("Older Scala Versions")
  )

  def talksSection = Section(
    "talksSection",
    div("talksSection")
  )

  def inTheWildSection = Section(
    "inTheWildSection",
    div("inTheWildSection")
  )

  def scaladocSection = Section(
    "Scaladoc",
    div("Scaladoc")
  )

  def changelogSection = Section(
    "Changelog",
    div("Changelog")
  )

  def unstableVersionsSection = Section(
    "unstableVersionsSection",
    div("unstableVersionsSection"),
    List(unstableChangelogSection)
  )

  def unstableChangelogSection = Section(
    "unstableChangelogSection",
    div("unstableChangelogSection")
  )

}
