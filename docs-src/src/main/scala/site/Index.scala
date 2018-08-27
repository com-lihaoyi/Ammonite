package site

import scalatags.Text.all._
import ba.sake.hepek.html.structure.blog.Section
import ba.sake.hepek.html.component.GridComponents
import ba.sake.hepek.bootstrap3.component.BootstrapGridComponents
import templates.AmmoniteStaticPage
import utils.Imports._

object Index extends AmmoniteStaticPage {
  import grid._

  private val replLink =
    s"[REPL](${AmmoniteRepl.ammoniteReplSection.ref})"
  private val scalaScriptsLink =
    s"[Scala Scripts](${AmmoniteScripts.ammoniteScriptsSection.ref})"
  private val shellLink =
    s"[systems shell](${AmmoniteShell.ammoniteShellSection.ref})"

  override def pageTitle = "Home"

  override def pageContent = row(
    third1(),
    third2(
      div(cls := "well", style := "margin-top: 11px;")(
        md(s"""
          Ammonite lets you use the [Scala language](https://www.scala-lang.org/)
            for scripting purposes: in the $replLink,
            as $scalaScriptsLink,
            as a [library to use in existing projects](${AmmoniteOps.ammoniteOpsSection.ref})
            or as a standalone $shellLink.
        """)
      ),
      // logos
      table(cls := "table table-hover")(
        for ((faIcon, section, desc, textt) <- logos)
          yield
            tr(
              td(
                div(cls := "text-center")(
                  i(cls := s"fa $faIcon",
                    fontSize := "48px",
                    aria.hidden := "true"),
                  div(
                    h3(hyperlink(section.ref)(section.name))
                  )
                )
              ),
              td(style := "vertical-align: middle;")(p(b(desc), ".", textt))
            )
      ),
      md(s"""
        Ammonite is a project by [Li Haoyi](http://www.lihaoyi.com/). 
        If you use Ammonite and enjoyed it, please chip in to support its development
          at [https://www.patreon.com/lihaoyi](https://www.patreon.com/lihaoyi).

        Any amount will help us develop Ammonite into the best possible REPL 
          and script runner for the Scala community!

        The goal of Ammonite is to liberate your Scala code from heavyweight "projects", 
          using the lightweight Ammonite runtime. 
        If you want to run some Scala, open the 
          $replLink and run it, interactively!
        If you want to run it later, save it into some $scalaScriptsLink
          and run those later.
        
        For a video overview of the project and it's motivation, check out this talk:
      """),
      iframe(
        src := "https://player.vimeo.com/video/148552858",
        width := 800,
        height := 600,
        attr("frameborder") := 0,
        attr("webkitallowfullscreen") := 1,
        attr("mozallowfullscreen") := 1,
        attr("allowfullscreen") := 1
      ),
      md(s"""
        ---
        If you are already working in Scala,
          you no longer have to drop down to Python or Bash for your scripting needs:
          you can use $scalaScriptsLink for your scripting needs, 
          and avoid the overhead of working in multiple languages.

        Each of the above projects is usable standalone; 
          click on the links to jump straight to their docs, 
          or scroll around and browse through the navigation bar on the top. 
        If you're wondering what you can do *with* Ammonite,
          there is an [Ammonite Cookbook](${AmmoniteCookbook.ammoniteCookbookSection.ref})
          which contains a bunch of fun things that you can do, whether in the
          interactive $replLink
          or in $scalaScriptsLink.

        You can also take a look at how people are using Ammonite 
          [in the wild](${Reference.inTheWildSection.ref}), to see what people are doing with it. 
        And there are more talks available [here](${Reference.talksSection}).

        The bulk of this page describes the latest stable release of Ammonite, `1.1.2` //TODO.  
        If you're willing to live on the edge,
          we also publish [Unstable Versions](${Reference.unstableVersionsSection.ref})
          from any commits that get pushed
          or pull-requests that land in the master branch:

        Sponsored by:
        - [Rally Health](http://engineering.rallyhealth.com/): putting healthcare 
            in the hands of consumers, we work on critical applications at a 
            massive scale in Scala, C# and JavaScript. If you're smart and like moving fast, 
            [work with us](https://www.rallyhealth.com/company/careers/).
      """)
    ),
    third3()
  )

  def logos = Seq(
    (
      "fa-refresh",
      AmmoniteRepl.ammoniteReplSection,
      "A Modernized Scala REPL",
      """
        With syntax highlighting,
        multi-line editing, ability to load maven artifacts
        directly in the REPL, and many other quality-of-life
        improvements missing in the default Scala REPL.
      """
    ),
    (
      "fa-file-text-o",
      AmmoniteScripts.ammoniteScriptsSection,
      "Lightweight Programming in Scala",
      """
        Create scripts that you can
        run easily from the command line, without the overhead of
        setting up a "project" or waiting for SBT's slow startup times.
      """
    ),
    (
      "fa-gears",
      AmmoniteOps.ammoniteOpsSection,
      "A Rock-solid Filesystem Library for Scala",
      """
        Deal with the filesystem
        easily from your existing Scala projects or applications, as easily
        as you would from a Bash or Python script.
      """
    ),
    (
      "fa-terminal",
      AmmoniteShell.ammoniteShellSection,
      "A modern replacement for the Bash system shell",
      """
        Provides a systems shell in the high-level Scala language, letting you
        seamlessly mix system operations with real code without the hassle or
        the frustration of trying to write complex code in Bash.
      """
    )
  )

}

object grid extends BootstrapGridComponents {
  import GridComponents._

  val halfRatio  = Ratio(List(1, 1))
  val thirdRatio = Ratio(List(3, 6, 3))
  val ratios     = Ratios(halfRatio, thirdRatio)
  override def screenRatios = super.screenRatios.copy(
    lg = ratios,
    md = Option(ratios),
    sm = None,
    xs = None
  )
}
