package ammonite.repl


import ammonite.interp.Interpreter
import ammonite.runtime._
import ammonite.util.Util._
import ammonite.util._

import scala.collection.mutable

class SessionApiImpl(frames0: StableRef[List[Frame]]) extends Session{
  def frames = frames0()
  val namedFrames = mutable.Map.empty[String, List[Frame]]

  def childFrame(parent: Frame) = new Frame(
    new SpecialClassLoader(
      parent.classloader,
      parent.classloader.classpathSignature
    ),
    new SpecialClassLoader(
      parent.pluginClassloader,
      parent.pluginClassloader.classpathSignature
    ),
    parent.imports,
    parent.classpath
  )

  def save(name: String = "") = {
    if (name != "") namedFrames(name) = frames
    frames0() = childFrame(frames.head) :: frames
  }

  def pop(num: Int = 1) = {
    var next = frames
    for(i <- 0 until num){
      if (next.tail != Nil) next = next.tail
    }
    val out = SessionChanged.delta(frames.head, next.head)
    frames0() = childFrame(next.head) :: next
    out
  }
  
  def load(name: String = "") = {
    val next = if (name == "") frames.tail else namedFrames(name)
    val out = SessionChanged.delta(frames.head, next.head)
    frames0() = childFrame(next.head) :: next
    out
  }

  def delete(name: String) = {
    namedFrames.remove(name)
  }
}
class ReplApiImpl(val interp: Interpreter,
                  width0: => Int,
                  height0: => Int,
                  colors0: Ref[Colors],
                  prompt0: Ref[String],
                  frontEnd0: Ref[FrontEnd],
                  history0: => History,
                  sess0: Session,
                  replArgs0: Seq[Bind[_]]) extends DefaultReplAPI{
  import interp._

  def lastException = interp.lastException

  def imports = eval.imports.toString
  val colors = colors0
  val prompt = prompt0
  val frontEnd = frontEnd0

  implicit def tprintColorsImplicit = pprint.TPrintColors(
    typeColor = colors().`type`()
  )
  implicit val codeColorsImplicit = new CodeColors{
    def comment = colors().comment()
    def `type` = colors().`type`()
    def literal = colors().literal()
    def keyword = colors().keyword()
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
      .foreach(printer.out)
    printer.out(newLine)
  }

  def search(target: scala.reflect.runtime.universe.Type) = compilerManager.search(target)
  def compiler = interp.compilerManager.compiler.compiler
  def newCompiler() = compilerManager.init(force = true)
  def fullHistory = storage.fullHistory()
  def history = history0


  def width = width0
  def replArgs = replArgs0.toVector
  def height = height0

  object sess extends Session {
    def frames = frames
    def save(name: String) = sess0.save(name)
    def delete(name: String) = sess0.delete(name)

    def pop(num: Int = 1) = {
      val res = sess0.pop(num)
      res
    }
    def load(name: String = "") = {
      val res = sess0.load(name)
      res
    }
  }

}
