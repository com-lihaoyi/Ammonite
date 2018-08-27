package site

import scalatags.Text.all._
import ammonite.ops._
import ba.sake.hepek.html.structure.blog.Section
import templates.AmmoniteBlogPage
import utils.Imports._
import utils.Sample._

object AmmoniteRepl extends AmmoniteBlogPage {

  override def pageTitle    = "REPL"
  override def postSections = List(ammoniteReplSection)

  val ammoniteTests = pwd / up / 'amm / 'repl / 'src / 'test / 'scala / 'ammonite / 'session
  val advancedTests = ammoniteTests / "AdvancedTests.scala"

  def bashSnippet = chl.bash.withPrompt("$")

  def ammoniteReplSection = Section(
    "Ammonite REPL",
    p(
      md(s"""
        The **Ammonite-REPL** is an improved Scala REPL, re-implemented from first principles.
        It is much more featureful than the default REPL and comes
          with a lot of [ergonomic improvements](${featuresSection.ref})
          and [configurability](${configurationSection.ref}) that may be familiar to
          people coming from IDEs or other REPLs such as
          [IPython](https://ipython.org/) or [Zsh](http://www.zsh.org/).

        It can be combined with [Ammonite-Ops](${AmmoniteOps.ref}) 
          to replace Bash as your systems shell,
          but also can be used alone as a 
          [superior](${featuresSection.ref}) version of the default Scala REPL,
          or as a @sect.ref("Debugging", "debugging tool"), or for many other
          @sect.ref("Ammonite Cookbook", "fun and interesting things")!

        If you want to use Ammonite as a plain Scala shell, download the standalone
          Ammonite @ammonite.Constants.version executable for Scala 2.12
          (also available for @sect.ref{Older Scala Versions}):
        """),
      bashSnippet(replCurl),
      md(s"""
          You can also try out Ammonite @ammonite.Constants.version in an existing
          SBT project. To do so, add the following to your @code{build.sbt}"""),
      chl.scala(
        """
          libraryDependencies += {
            val version = scalaBinaryVersion.value match {
              case "2.10" => "1.0.3"
              case _ ⇒ "@ammonite.Constants.version"
            }
            "com.lihaoyi" % "ammonite" % version % "test" cross CrossVersion.full
          }

          sourceGenerators in Test += Def.task {
            val file = (sourceManaged in Test).value / "amm.scala"
            IO.write(file, \"\""object amm extends App { ammonite.Main.main(args) }\"\"")
            Seq(file)
          }.taskValue
        """
      )
    ),
    List(featuresSection, configurationSection)
  )

  def featuresSection = Section(
    "Features",
    div(
      "TODO"
    ),
    List(prettyPrintedOutputSection)
  )

  def prettyPrintedOutputSection = Section(
    "Pretty-printed output",
    frag(
      chl
        .scala( // TODO make this nicer
          extractFileSnippet(advancedTests,
                             List("'pprint", "@"),
                             List("\"\"\""))._3
        )
    )
  )

  def configurationSection = Section(
    "Configuration",
    div()
  )

}
