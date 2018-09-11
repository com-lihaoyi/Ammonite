package templates

import java.time.LocalDate

import ba.sake.hepek.anchorjs.AnchorjsDependencies
import ba.sake.hepek.bootstrap3.statik.BootstrapStaticPage
import ba.sake.hepek.html.structure._
import ba.sake.hepek.theme.bootstrap3.HepekBootstrap3BlogPage
import ba.sake.hepek.prismjs.{PrismDependencies, PrismSettings, Themes}
import utils.Imports._
import resources._

trait AmmoniteBlogPage extends AmmoniteStaticPage with HepekBootstrap3BlogPage {
  override def blogSettings =
    super.blogSettings
      .withAuthor("Li Haoyi")
      .withCreateDate(LocalDate.now)
}

trait AmmoniteStaticPage
    extends BootstrapStaticPage
    with PrismDependencies
    with AnchorjsDependencies {

  override def siteSettings =
    super.siteSettings
      .withName("Ammonite")
      .withIndexPage(site.Index)
      .withMainPages(site.Index,
                     site.AmmoniteRepl,
                     site.AmmoniteScripts,
                     site.AmmoniteOps,
                     site.AmmoniteShell,
                     site.AmmoniteCookbook,
                     site.Reference)
      .withFaviconNormal(images.image("favicon.png").ref)
  // .withFaviconInverted(images.image("favicon-mini.png").ref)

  override def prismSettings: PrismSettings =
    super.prismSettings.withTheme(Themes.Tomorrow)

  override def styleURLs = super.styleURLs ++ List(
    styles.css("main").ref,
    "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.css"
  )
  override def bootstrapDependencies =
    super.bootstrapDependencies.withCssDependencies(
      Dependencies()
        .withDeps(
          Dependency("cosmo/bootstrap.min.css", bootstrapSettings.version, "bootswatch")
        )
    )

  override def scriptURLs = super.scriptURLs :+ scripts.js("main").ref
}
