package templates

import java.time.LocalDate

import ba.sake.hepek.anchorjs.AnchorjsDependencies
import ba.sake.hepek.bootstrap3.statik.BootstrapStaticPage
import ba.sake.hepek.theme.bootstrap3.HepekBootstrap3BlogPage
import ba.sake.hepek.html._
import structure._
import component._
import ba.sake.hepek.prismjs.{PrismDependencies, Themes}
import utils.Imports._
import resources._

trait AmmoniteBlogPage extends AmmoniteStaticPage with HepekBootstrap3BlogPage {
  override def postAuthor     = Option("Li Haoyi")
  override def postCreateDate = Option(LocalDate.now)
}

trait AmmoniteStaticPage
    extends BootstrapStaticPage
    with BasicComponents
    with PrismDependencies
    with AnchorjsDependencies {

  override def siteSettings = SiteSettings(
    "Ammonite",
    site.Index,
    List(
      site.Index,
      site.AmmoniteRepl,
      site.AmmoniteScripts,
      site.AmmoniteOps,
      site.AmmoniteShell,
      site.AmmoniteCookbook,
      site.Reference
    ),
    Option(images.image("favicon.png").ref),
    Option(images.image("favicon-mini.png").ref)
  )

  override def prismTheme: String = Themes.Tomorrow

  override def styleURLs = super.styleURLs ++ List(
    styles.css("main").ref,
    "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.css"
  )

  override def scriptURLs = super.scriptURLs :+ scripts.js("main").ref
}
