package site

import scalatags.Text.all._
import ammonite.ops._
import ammonite.Constants
import ba.sake.hepek.implicits._
import utils._
import Imports._
import ba.sake.hepek.utils.StringUtils

object AmmoniteRepl extends templates.AmmoniteBlogPage {

  override def pageSettings =
    super.pageSettings.withTitle("REPL")
  override def blogSettings =
    super.blogSettings.withSections(ammoniteReplSection)

  val ammoniteTests = pwd / up / 'amm / 'repl / 'src / 'test / 'scala / 'ammonite / 'session
  val advancedTests = ammoniteTests / "AdvancedTests.scala"

  val srcSbtConfig =
    """
      // Optional, required for the `source` command to work
      (fullClasspath in Test) ++= {
        (updateClassifiers in Test).value
          .configurations
          .find(_.configuration == Test.name)
          .get
          .modules
          .flatMap(_.artifacts)
          .collect{case (a, f) if a.classifier == Some("sources") => f}
      }
    """

  def bashSnippet = chl.bash.withPrompt("$")

  def ammoniteReplSection = Section(
    "Ammonite REPL",
    p(
      s"""
        The **Ammonite-REPL** is an improved Scala REPL, re-implemented from first principles.
        It is much more featureful than the default REPL and comes
          with a lot of [ergonomic improvements](${featuresSection.ref})
          and [configurability](${configurationSection.ref}) that may be familiar to
          people coming from IDEs or other REPLs such as
          [IPython](https://ipython.org/) or [Zsh](http://www.zsh.org/).

        It can be combined with [Ammonite-Ops](${AmmoniteOps.ref}) 
          to replace Bash as your systems shell,
          but can also be used alone as a 
          [superior](${featuresSection.ref}) version of the default Scala REPL,
          or as a [debugging tool](${debuggingSection.ref}), or for many other
          [fun and interesting things](${AmmoniteCookbook.ref})!

        If you want to use Ammonite as a plain Scala shell, download the standalone
          Ammonite ${Constants.version} executable for Scala 2.12
          (also available for [Older Scala Versions](${Reference.olderScalaVersionsSection.ref})):
      """.md,
      bashSnippet(Sample.replCurl),
      s"""
         Or to try out the [latest features](${Reference.unstableChangelogSection.ref}) 
         in our [Unstable Release](${Reference.unstableVersionsSection.ref}) ${Constants.version}:
       """.md,
      bashSnippet(Sample.unstableCurl),
      "This will give you access to the Ammonite-REPL:",
      image(resources.images.image("GettingStarted.png").ref),
      s"""
        With [Pretty Printing](${prettyPrintedOutputSection.ref}),
         [Syntax Highlighting](${syntaxHighlightingSection.ref}) for input and output,
         [Artifact Loading](${importIvySection.ref}) in-REPL, and all the other nice
         [Features](${featuresSection.ref})!
      
        If you're not sure what to do with Ammonite, 
          check out the [Ammonite Cookbook](${AmmoniteCookbook.ref}) for some fun ideas!
      
        If you want to use Ammonite as a filesystem shell, 
          take a look at [Ammonite Shell](${AmmoniteShell.ref}).
          
        If you want some initialization code available to the REPL, you can add
          it to your `~/.ammonite/predef.sc`.
          
        If you have any questions, come hang out on the 
           [mailing list or gitter channel](${Reference.communitySection.ref}) and get help!
           
        ---
        You can also try out Ammonite ${Constants.version} in an existing
          SBT project. To do so, add the following to your `build.sbt`
      """.md,
      chl.scala(
        StringUtils.unindent(s"""
          libraryDependencies += {
            val version = scalaBinaryVersion.value match {
              case "2.10" => "1.0.3"
              case _ ⇒ "${Constants.version}"
            }
            "com.lihaoyi" % "ammonite" % version % "test" cross CrossVersion.full
          }

          sourceGenerators in Test += Def.task {
            val file = (sourceManaged in Test).value / "amm.scala"
            IO.write(file, \"\""object amm extends App { ammonite.Main.main(args) }\"\"")
            Seq(file)
          }.taskValue
        """) + StringUtils.unindent(srcSbtConfig)
      ),
      s"""
        Or to try out the [latest features](${Reference.unstableChangelogSection.ref}) in our
          [Unstable Release](${Reference.unstableVersionsSection.ref}) ${Constants.version}:
      """.md,
      chl.scala(
        s"""
          libraryDependencies += "com.lihaoyi" % "ammonite" % "${Constants.unstableVersion}" % "test" cross CrossVersion.full
        """
      ),
      "After that, simply hit",
      bashSnippet("sbt projectName/test:run"),
      "or if there are other main methods in the `Test` scope".md,
      bashSnippet("sbt projectName/test:run-main amm"),
      "to activate the Ammonite REPL.",
      """
        You can also pass a string to the `Main` call containing any
          commands or imports you want executed at the start of every run, 
          along with [other configuration](http://ammonite.io/api/repl/index.html#ammonite.repl.Main).
      
        If you want Ammonite to be available in all projects, simply add the
          above snippet to a new file `~/.sbt/0.13/global.sbt`.
      """.md
    ),
    List(featuresSection, configurationSection, embeddingSection)
  )

  /* Features */
  def featuresSection = Section(
    "Features",
    "Ammonite-REPL supports many more features than the default REPL, including:",
    List(
      prettyPrintedOutputSection,
      editingSection,
      magicImportsSection,
      builtinsSection,
      saveLoadSessionSection,
      saveLoadSessionSection,
      superiorAutocompleteSection,
      interruptingRunAwayExecutionSection,
      compilerCrashRobustnessSection,
      otherFixesSection
    )
  )

  def prettyPrintedOutputSection = Section(
    "Pretty-printed output",
    frag(
      ammSnippet(advancedTests, List("'pprint", "@")),
      s"""
        Ammonite-REPL uses [PPrint](http://www.lihaoyi.com/PPrint/) to display its output by default. 
        That means that everything is nicely formatted to fit within the width of the terminal, and is copy-paste-able!

        By default, Ammonite [truncates](${configurableTruncationSection.ref})
          the pretty-printed output to avoid flooding your terminal. 
        If you want to disable truncation, call `show(...)` on your expression to
          pretty-print its full output. 
        You can also pass in an optional `height = ...` parameter to control how much you want to show before truncation.
      """.md
    ),
    List(configurableTruncationSection)
  )

  def configurableTruncationSection =
    Section(
      "Configurable Truncation",
      div(
        ammSnippet(advancedTests, List("'truncation", "@")),
        s"""
          Ammonite-REPL intelligently truncates your output when it's beyond a certain size. 
            You can request for the full output to be printed on-demand, print a certain number of lines, 
            or even change the implicit `pprintConfig` so subsequent lines all use your new configuration.
        """.md
      )
    )

  def editingSection =
    Section(
      "Editing",
      "Ammonite by default ships with a custom implementation of readline, which provides...".md,
      List(
        syntaxHighlightingSection,
        multiLlineEditingSection,
        desktopKeyBindingsSection,
        consoleKeyBindingsSection,
        historySearchSection,
        blockInputSection,
        undoRedoSection
      )
    )

  def syntaxHighlightingSection =
    Section(
      "Syntax Highlighting",
      div(
        image(resources.images.image("Highlighting.png").ref),
        s"""
          Ammonite syntax highlights both the code you're entering as well as any
            output being echoed in response. This should make it much easier to
            work with larger snippets of input.
            
           All colors are configurable, and you can easily turn off colors entirely
            via the [Configuration](${configurationSection.ref}).
        
           Stack traces are similarly highlighted, for easier reading:
         """.md,
        image(resources.images.image("ColoredTraces.png").ref)
      )
    )

  def multiLlineEditingSection = Section(
    "Multiline editing",
    """
      You can use the <kbd>Up</kbd> and <kbd>Down</kbd> arrows to navigate between lines within your snippet. 
      `Enter` only executes the code when you're
        on the last line of a multiline snippet, meaning you can take your
        time, space out your code nicely, and fix any mistakes anywhere in your snippet. 
      History is multiline too, meaning re-running a multiline
        snippet is trivial, even with tweaks.
        
      Long gone are the days where you're desperately trying to cram
        everything in a single line, or curse quietly when you notice a mistake
        in an earlier line you are no longer able to fix. No more painstakingly
        crafting a multiline snippet, and then having to fish it
        line by individual line out of the history so you can run it again!
    """.md
  )

  def desktopKeyBindingsSection = Section(
    "Desktop keybindings",
    div(
      img(src := resources.images.image("Editing.gif").ref,
          display.block,
          marginLeft.auto,
          marginRight.auto,
          height := 172),
      """
        You can use <kbd>Alt-Left</kbd>/<kbd>Right</kbd> to move forward/backwards by one
          word at a time or hold down <kbd>Shift</kbd> to select text to delete. 
        These compose as you'd be used to: e.g. <kbd>Shift-Up</kbd> selects all the text
          between your current cursor and the same column one row up.
          
        <kbd>Tab</kbd> and <kbd>Shift-Tab</kbd> now work to block-indent and -dedent
          sections of code, as you'd expect in any desktop editor like Sublime Text or IntelliJ. 
        This further enhances the multiline editing experience, 
          letting your nicely lay-out your more-complex REPL commands
          the same way you'd format code in any other editor.
      """.md
    )
  )

  def consoleKeyBindingsSection =
    Section(
      "Console keybindings",
      """
        All the readline-style navigation hotkeys like <kbd>Ctrl-W</kbd> to delete a
          word or <kbd>Esc-Left</kbd>/<kbd>Right</kbd> to navigate one word left/right still
          work. If you're comfortable with consoles like Bash, Python, IPython or
          even the default Scala console, you should have no trouble as all the
          exact same hotkeys work in Ammonite.
      """.md
    )

  def historySearchSection = Section(
    "History Search",
    div(
      img(src := resources.images.image("HistorySearch.gif").ref,
          width := "445px",
          marginRight.auto,
          marginLeft.auto,
          display.block),
      """
        Apart from browsing your command-line history with <kbd>UP</kbd>, you can
          also perform a history search by entering some search term and *then*
          pressing <kbd>UP</kbd>. That will pull up the most recent history line with
          that term in it, underlined. You can continue to press <kbd>UP</kbd> or
          <kbd>DOWN</kbd> to cycle through the matches, or <kbd>Backspace</kbd> or
          continue typing characters to refine your search to what you want.
        
        You can press <kbd>TAB</kbd>, or any other command character (<kbd>LEFT</kbd>,
          <kbd>RIGHT</kbd>, ...) to end the search and let you continue working with
          the currently-displayed command. Pressing <kbd>ENTER</kbd> will end the
          search and immediately submit the command to be run.
        
        You can also kick off a history search using <kbd>Ctrl-R</kbd>, and use
          <kbd>Ctrl-R</kbd> to cycle through the matches.
      """.md
    )
  )

  def blockInputSection =
    Section(
      "Block Input",
      div(
        """
          To enter block input (many independent lines all at once) into the
            Ammonite-REPL, simply wrap the multiple lines in curly braces
            `{ ... }`, and Ammonite will wait until you close it before evaluating the contents:
        """.md,
        ammSnippet(advancedTests, List("'unwrapping", "@")),
        """
        As you can see, the contents of the `{ ... }` block are
        unwrapped and evaluated as top-level statements. You can use this to
        e.g. declare mutually recursive functions or classes &
        companion-objects without being forced to squeeze everything onto a single line.

        If you don't want this un-wrapping behavior, simply add another set of
          curlies and the block will be evaluated as a normal block, to a single
          expression:

      """.md,
        ammSnippet(advancedTests, List("'forceWrapping", "@"))
      )
    )

  def undoRedoSection =
    Section(
      "Undo & Redo",
      div(
        img(src := resources.images.image("UndoRedo.gif").ref,
            display.block,
            marginLeft.auto,
            marginRight.auto,
            height := 154),
        """
          The Ammonite command-line editor allows you to undo and re-do portions of your edits:
            - <kbd>Ctrl -</kbd>: Undo last change
            - <kbd>Alt/Esc -</kbd>: Redo last change
            
          Each block of typing, deletes, or navigation counts as one undo. 
          This should make it much more convenient to recover from botched copy-pastes or bulk-deletions.
        """.md
      )
    )

  def magicImportsSection = Section(
    "Magic Imports",
    div(),
    List(importFileSection, importExecSection, importIvySection)
  )

  def importFileSection = Section(
    "import $file",
    div()
  )

  def importExecSection = Section(
    "import $exec",
    div()
  )

  def importIvySection = Section(
    "import $ivy",
    div()
  )

  def builtinsSection = Section(
    "Builtins",
    div()
  )

  def saveLoadSessionSection = Section(
    "Save/Load Session",
    div()
  )

  def superiorAutocompleteSection = Section(
    "Superior Autocomplete",
    div()
  )

  def interruptingRunAwayExecutionSection = Section(
    "Interrupting run-away execution with Ctrl-C",
    div()
  )

  def compilerCrashRobustnessSection = Section(
    "Compiler-crash Robustness",
    div()
  )

  def otherFixesSection = Section(
    "Other Fixes",
    div()
  )

  /* Configuration */
  def configurationSection = Section(
    "Configuration",
    div(),
    List(refsSection, compilerFlagsSection, jvmFlagsSection)
  )

  def refsSection = Section(
    "Refs",
    div()
  )

  def compilerFlagsSection = Section(
    "Compiler Flags",
    div()
  )

  def jvmFlagsSection = Section(
    "JVM Flags",
    div()
  )

  /* Embedding */
  def embeddingSection = Section(
    "Embedding",
    div(),
    List(instantiatingAmmoniteSection, debuggingSection, remoteReplSection)
  )

  def instantiatingAmmoniteSection = Section(
    "Instantiating Ammonite",
    div()
  )

  def debuggingSection = Section(
    "Debugging",
    div()
  )

  def remoteReplSection = Section(
    "Remote REPL",
    div()
  )

}
