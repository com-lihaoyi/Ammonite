package ammonite.repl

import ammonite.interp.api.APIHolder
import ammonite.repl.api.{History, ReplAPI, ReplLoad}
import ammonite.util.{Bind, _}
import ammonite.util.Util.newLine

import scala.reflect.runtime.universe._

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.classTag
import ammonite.repl.api.Clipboard
import pprint.PPrinter
import java.io.OutputStream
import java.io.PrintStream

object ReplBridge extends APIHolder[ReplAPI]

object ReplExtras {

  /**
   * Get the `Type` object of [[T]]. Useful for finding
   * what its methods are and what you can do with it
   */
  def typeOf[T: WeakTypeTag] = scala.reflect.runtime.universe.weakTypeOf[T]
  /**
   * Get the `Type` object representing the type of `t`. Useful
   * for finding what its methods are and what you can do with it
   *
   */
  def typeOf[T: WeakTypeTag](t: => T) = scala.reflect.runtime.universe.weakTypeOf[T]

  /**
   * Display help text if you don't know how to use the REPL
   */
  def help =
    """Welcome to the Ammonite Scala REPL! Enter a Scala expression and it will be evaluated.
      |All your standard Bash hotkeys should work for navigating around or editing the line
      |being entered, as well as some GUI hotkeys like alt-shift-left/right to select words
      |to replace. Hit <tab> to autocomplete possible names.
      |
      |For a list of REPL built-ins and configuration, use `repl.<tab>`. For a more detailed
      |description of how to use the REPL, check out https://lihaoyi.github.io/Ammonite
    """.stripMargin.trim

  def combinePrints(iters: Iterator[String]*) = {
    iters.toIterator
         .filter(_.nonEmpty)
         .flatMap(Iterator(newLine) ++ _)
         .drop(1)
  }

  /**
   * Lets you configure the pretty-printing of a value. By default, it simply
   * disables truncation and prints the entire thing, but you can set other
   * parameters as well if you want.
   */
  def show(t: Any,
           width: Integer = null,
           height: Integer = 9999999,
           indent: Integer = null)(implicit api: ReplAPI) = {

    api.pprinter()
      .tokenize(
        t,
        width = if (width == null) api.pprinter().defaultWidth else width,
        height = if (height == null) api.pprinter().defaultHeight else height,
        indent = if (indent == null) api.pprinter().defaultIndent else indent
      )
      .map(_.render)
      .foreach(api.printer.outStream.print)
    api.printer.outStream.print(newLine)
  }

  trait PrintHook {
    def print[T](value: => T,
                  ident: String,
                  custom: Option[String])
                 (implicit tprint: pprint.TPrint[T],
                  tcolors: pprint.TPrintColors,
                  api: ReplAPI,
                  classTagT: ClassTag[T]): Option[Iterator[String]]
  }

  def print[T: pprint.TPrint](value: => T,
                              ident: String,
                              custom: Option[String])
                             (implicit tcolors: pprint.TPrintColors,
                              api: ReplAPI,
                              classTagT: ClassTag[T] = null): Iterator[String] = {
    val hookedPrintIt = api
      .printHooks
      .iterator
      .flatMap { hook =>
        hook
          .print(value, ident, custom)(implicitly[pprint.TPrint[T]], tcolors, api, classTagT)
          .iterator
      }
    val hookedPrintOpt =
      if (hookedPrintIt.hasNext) Some(hookedPrintIt.next())
      else None

    hookedPrintOpt.getOrElse {
      defaultPrint[T](value, ident, custom)
    }
  }
  def defaultPrint[T: pprint.TPrint](value: => T,
                                     ident: String,
                                     custom: Option[String])
                                    (implicit tcolors: pprint.TPrintColors,
                                     api: ReplAPI,
                                     classTagT: ClassTag[T] = null): Iterator[String] = {

    // Here we use ClassTag to detect if T is an Unit.
    //
    // The default value null suppresses the compilation error when T is a singleton type,
    // which can't provide a ClassTag.
    //
    // We don't use `asUnit: T =:= Unit = null` because that approach does not work
    // when T is Nothing.
    // See https://github.com/scala/bug/issues/10393 for further information.
    //
    // We don't use WeakTypeTag or TypeTag because those type classes are too heavy-weight,
    // as Scalac will generate a huge amount of code for creating a TypeTag for refinement types.
    // See https://github.com/lihaoyi/Ammonite/issues/649 for further information.
    //
    // We do not check `value == ()`, because that would force evaluation of `value`, which
    // may be defined as a `lazy val` which the user explicitly does not want to evaluate
    val isUnit = classTagT == classTag[Unit]

    if (isUnit) Iterator()
    else {

      // Pre-compute how many lines and how many columns the prefix of the
      // printed output takes, so we can feed that information into the
      // pretty-printing of the main body
      val prefix = new pprint.Truncated(
        Iterator(
          api.ammColors().ident()(ident).render, ": ",
          implicitly[pprint.TPrint[T]].render(tcolors), " = "
        ),
        api.pprinter().defaultWidth,
        api.pprinter().defaultHeight
      )
      val output = mutable.Buffer.empty[fansi.Str]

      prefix.foreach(output += _)

      val rhs = custom match {
        case None =>
          api.pprinter().tokenize(
            value,
            height = api.pprinter().defaultHeight - prefix.completedLineCount,
            initialOffset = prefix.lastLineLength
          ).toStream
        case Some(s) => Seq(api.pprinter().colorLiteral(s))
      }

      output.iterator.map(_.render) ++ rhs.map(_.render)
    }
  }
  def printDef(definitionLabel: String, ident: String)(implicit api: ReplAPI) = {
    Iterator(
      "defined ", api.ammColors().`type`()(definitionLabel).render, " ",
      api.ammColors().ident()(ident).render
    )
  }
  def printImport(imported: String)(implicit api: ReplAPI) = {
    Iterator(api.ammColors().`type`()("import ").render, api.ammColors().ident()(imported).render)
  }

  implicit def pprinterImplicit(implicit api: ReplAPI) = api.pprinter()

  /**
   * Controls how things are pretty-printed in the REPL. Feel free
   * to shadow this with your own definition to change how things look
   */
  implicit def tprintColorsImplicit(implicit api: ReplAPI) = pprint.TPrintColors(
    typeColor = api.ammColors().`type`()
  )
  implicit def codeColorsImplicit(implicit api: ReplAPI): CodeColors = new CodeColors{
    def comment = {
      pprint.log(api)
      pprint.log(ReplBridge)
      api
        .ammColors()
        .comment()
    }
    def `type` = api.ammColors().`type`()
    def literal = api.ammColors().literal()
    def keyword = api.ammColors().keyword()
    def ident = api.ammColors().ident()
  }

  private final case class Data(
    pprinter: Ref[PPrinter],
    printHooks: List[PrintHook]
  )

  private lazy val bw = Colors.BlackWhite
  private lazy val default = Colors.Default

  implicit class ReplAPIExtensions(private val api: ReplAPI) extends AnyVal {
    def typeOf[T: WeakTypeTag] = ReplExtras.typeOf[T]
    def typeOf[T: WeakTypeTag](t: => T) = ReplExtras.typeOf[T](t)
    def help = ReplExtras.help

    def show(t: Any,
             width: Integer = null,
             height: Integer = 9999999,
             indent: Integer = null) =
      ReplExtras.show(t, width, height, indent)(api)

    def clipboard: Clipboard =
      Clipboard.clipboardImpl

    /**
     * History of commands that have been entered into the shell, including
     * previous sessions
     */
    def fullHistory: History =
      new History(api.fullRawHistory)

    /**
     * History of commands that have been entered into the shell during the
     * current session
     */
    def history: History =
      new History(api.rawHistory)


    private def data: Data = {
      if (api.data == null) {
        ReplExtras.synchronized {
          if (api.data == null) {
            val pprinterRef: Ref[PPrinter] = Ref.live(() =>
              pprint.PPrinter.Color.copy(
                defaultHeight = api.height / 2,
                defaultWidth = api.width,
                colorLiteral = ammColors().literal(),
                colorApplyPrefix = ammColors().prefix()
                // TODO Add that back somehow
                // additionalHandlers = PPrints.replPPrintHandlers
              )
            )
            api.setData(Data(
              pprinterRef,
              Nil
            ))
          }
        }
      }
      api.data.asInstanceOf[Data]
    }

    def ammColors() =
      api.getColors match {
        case ammonite.repl.api.Colors.BLACKWHITE => bw
        case _ => default
      }

    def colors: ammonite.repl.api.Colors =
      api.getColors
    def colors_=(colors: ammonite.repl.api.Colors): Unit =
      api.setColors(colors)

    def pprinter: Ref[PPrinter] =
      data.pprinter

    /**
     * Read/writable prompt for the shell. Use this to change the
     * REPL prompt at any time!
     */
    def prompt: String =
      api.getPrompt
    def prompt_=(prompt: => String) =
      api.setPrompt(() => prompt)

    /**
     * The front-end REPL used to take user input. Modifiable!
     */
    def frontEnd: ammonite.repl.api.FrontEnd =
      api.getFrontEnd
    def frontEnd_=(frontEnd: ammonite.repl.api.FrontEnd) =
      api.setFrontEnd(frontEnd)

    /**
     * Access the compiler to do crazy things if you really want to!
     */
    def compiler: scala.tools.nsc.Global =
      api.objCompiler.asInstanceOf[scala.tools.nsc.Global]

    /**
      * Access the presentation compiler to do even crazier things if you really want to!
      */
    def interactiveCompiler: scala.tools.nsc.interactive.Global =
      api.objPressy.asInstanceOf[scala.tools.nsc.interactive.Global]

    def load: ReplLoad =
      api.replLoad

    def printHooks: Seq[PrintHook] =
      data.printHooks
    def addPrintHook(hook: PrintHook): Unit =
      api.setData(data.copy(printHooks = hook :: data.printHooks))

  }

  implicit class SessionChangedExtensions(
    private val self: ammonite.repl.api.SessionChanged
  ) extends AnyVal {
    def removedImports: Set[scala.Symbol] =
      self.rawRemovedImports.map(Symbol(_)).toSet
    def addedImports: Set[scala.Symbol] =
      self.rawAddedImports.map(Symbol(_)).toSet
    def removedJars: Set[java.net.URL] =
      self.rawRemovedJars.toSet
    def addedJars: Set[java.net.URL] =
      self.rawAddedJars.toSet
  }
}
