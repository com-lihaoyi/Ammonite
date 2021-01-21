package ammonite.repl

import ammonite.ops.Internals
import ammonite.repl.api.{Clipboard, FrontEnd, FrontEndAPI, Session}
import ammonite.runtime._
import ammonite.util.Util._
import ammonite.util.{Frame => _, _}

import java.awt.Toolkit
import java.awt.datatransfer.{DataFlavor, StringSelection}
import java.util.Locale

import scala.collection.mutable

class SessionApiImpl(frames0: => StableRef[List[Frame]]) extends Session{
  def frames = frames0()
  val namedFrames = mutable.Map.empty[String, List[Frame]]

  def childFrame(parent: Frame) = new Frame(
    new SpecialClassLoader(
      parent.classloader,
      parent.classloader.classpathSignature,
      parent.classloader.specialLocalClasses
    ),
    new SpecialClassLoader(
      parent.pluginClassloader,
      parent.pluginClassloader.classpathSignature,
      parent.pluginClassloader.specialLocalClasses
    ),
    parent.imports,
    parent.classpath,
    parent.usedEarlierDefinitions
  )

  def save(name: String = "") = {
    if (name != "") namedFrames(name) = frames
    // freezing the frame will trigger the creation of a new one later on,
    // so that the saved one won't change any more
    frames.head.freeze()
    frames0() = frames
  }

  def pop(num: Int = 1) = {
    var next = frames
    for(i <- 0 until num){
      if (next.tail != Nil) next = next.tail
    }
    val out = SessionChanged.delta(frames.head, next.head)
    // freezing the current frame, so that the result of the current command,
    // that tangles with sessions, isn't added to it
    next.head.freeze()
    frames0() = next
    out
  }
  
  def load(name: String = "") = {
    val next = if (name == "") frames.tail else namedFrames(name)
    val out = SessionChanged.delta(frames.head, next.head)
    // freezing the current frame, so that the result of the current command,
    // that tangles with sessions, isn't added to it
    next.head.freeze()
    frames0() = next
    out
  }

  def delete(name: String) = {
    namedFrames.remove(name)
  }
}
trait ReplApiImpl extends FullReplAPI{

  implicit def tprintColorsImplicit = pprint.TPrintColors(
    typeColor = colors().`type`()
  )
  implicit val codeColorsImplicit = new CodeColors{
    def comment = colors().comment()
    def `type` = colors().`type`()
    def literal = colors().literal()
    def keyword = colors().keyword()
    def error = colors().error()
    def ident = colors().ident()
  }

  implicit val pprinter: Ref[pprint.PPrinter] = Ref.live(() =>
    pprint.PPrinter.Color.copy(
      defaultHeight = height / 2,
      defaultWidth = width,
      colorLiteral = colors().literal(),
      colorApplyPrefix = colors().prefix(),
      additionalHandlers = PPrints.replPPrintHandlers
    )
  )

  def show(t: Any) = show(t, null, 9999999, null)

  def printer: Printer

  override def show(t: Any,
                    width: Integer = null,
                    height: Integer = 9999999,
                    indent: Integer = null) = {

    pprinter()
      .tokenize(
        t,
        width = if (width == null) pprinter().defaultWidth else width,
        height = if (height == null) pprinter().defaultHeight else height,
        indent = if (indent == null) pprinter().defaultIndent else indent
      )
      .map(_.render)
      .foreach(printer.outStream.print)
    printer.outStream.print(newLine)
  }

  def sess: SessionApiImpl

  override def clipboard: Clipboard = ClipboardImpl
}
object ClipboardImpl extends Clipboard {

  private lazy val systemClipboard =
    Toolkit.getDefaultToolkit.getSystemClipboard

  override def read: String =
    Option(systemClipboard.getContents(null)) collect {
      case data if data.isDataFlavorSupported(DataFlavor.stringFlavor) =>
        data.getTransferData(DataFlavor.stringFlavor)
    } match {
      case Some(str: String) => str
      case _ => ""
    }

  override def write(data: Internals.Writable): Unit = {
    val newContents = new StringSelection(
      data.writeableData.map(new String(_)).mkString
    )
    systemClipboard.setContents(newContents, newContents)
  }
}

trait FrontEndAPIImpl extends FrontEndAPI {
  protected def parser: ammonite.compiler.iface.Parser
  def apply(name: String): FrontEnd =
    name.toLowerCase(Locale.ROOT) match {
      case "ammonite" => AmmoniteFrontEnd(parser)
      case "windows" => new FrontEnds.JLineWindows(parser)
      case "unix" => new FrontEnds.JLineUnix(parser)
      case _ => throw new NoSuchElementException(s"Front-end $name")
    }
}
