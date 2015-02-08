package ammonite.repl.eval
import acyclic.file
import ammonite.repl.Result
import ammonite.repl.eval.Preprocessor.Output

object Preprocessor{
  case class Output(code: String, printer: String)
}

/**
 * Converts REPL-style snippets into full-fledged Scala source files,
 * ready to feed into the compiler. Each source-string is turned into
 * three things:
 */
class Preprocessor{
  import scala.tools.reflect.ToolBox
  import scala.reflect.runtime.{currentMirror => m}
  val tb = m.mkToolBox()

  def Processor(cond: PartialFunction[(String, String, tb.u.Tree), Preprocessor.Output]) = {
    (code: String, name: String) => cond.lift(name, code, tb.parse(code))
  }

  def pprintSignature(ident: String) = s"""ReplBridge.shell.shellPPrint($ident, "$ident")"""

  def definedStr(definitionLabel: String, name: String) =
    s"""ReplBridge.shell.shellPrintDef("$definitionLabel", "$name") """

  def pprint(ident: String) = {
    pprintSignature(ident) +
      s""" + " = " + ammonite.pprint.PPrint($ident)"""
  }
  def DefProcessor(definitionLabel: String)(cond: PartialFunction[tb.u.Tree, String]) =
    (code: String, name: String) =>
      cond.lift(tb.parse(code)).map{
        name => Preprocessor.Output(code, definedStr(definitionLabel, name))
      }

  val ObjectDef = DefProcessor("object"){case m: tb.u.ModuleDef => m.name.toString}
  val ClassDef = DefProcessor("class"){
    case m: tb.u.ClassDef if !m.mods.hasFlag(tb.u.Flag.TRAIT)=> m.name.toString
  }
  val TraitDef =  DefProcessor("trait"){
    case m: tb.u.ClassDef if m.mods.hasFlag(tb.u.Flag.TRAIT) => m.name.toString
  }
  val DefDef = DefProcessor("function"){case m: tb.u.DefDef => m.name.toString}
  val TypeDef = DefProcessor("type"){case m: tb.u.TypeDef => m.name.toString}

  val PatVarDef = Processor { case (name, code, t: tb.u.ValDef) =>
    Preprocessor.Output(
      code,
      if (!t.mods.hasFlag(tb.u.Flag.LAZY)) pprint(t.name.toString)
      else pprintSignature(t.name.toString) + s""" + " = <lazy>" """
    )
  }
  val Expr = Processor{ case (name, code, tree) =>
    Preprocessor.Output(s"val $name = " + code, pprint(name))
  }
  val Import = Processor{ case (name, code, tree: tb.u.Import) =>
    Preprocessor.Output(code, '"'+code+'"')
  }

  val decls = Seq[(String, String) => Option[Preprocessor.Output]](
    ObjectDef, ClassDef, TraitDef, DefDef, TypeDef, PatVarDef, Import, Expr
  )

  def apply(code: String, wrapperId: Int): Result[Preprocessor.Output] = {
    val name = "res"+wrapperId

    util.Try(tb.parse(code)) match {
      case util.Failure(e) if e.getMessage.contains("expected but eof found") =>
        Result.Buffer(code)
      case util.Failure(e) => Result.Failure(e.toString)
      case util.Success(parsed) if parsed.toString == "<empty>" =>
        Result.Failure("")
      case util.Success(parsed) =>
        def handleTree(t: tb.u.Tree, c: String, name: String) = {
          decls.iterator.flatMap(_.apply(c, name)).next()
        }
        val allDecls = tb.parse(code) match{
          case b: tb.u.Block =>
            // _.pos.start doesn't work, we need to recurse into the trees
            // to find the position we want, which _.collect does for us
            val indices = b.children.map(_.collect{case t => t.pos.start}.min) :+ code.length
            val zipped = indices.zip(indices.tail).zip(b.children).zipWithIndex
            for((((start, end), tree), i) <- zipped) yield {
              handleTree(tree, code.substring(start, end), name + "_" + i)
            }
          case t => Seq(handleTree(t, code, name))
        }
        Result(
          allDecls.reduceOption((a, b) => Output(a.code+b.code, a.printer+ "+'\n'+" + b.printer)),
          "Don't know how to handle " + code
        )
      }
  }
}
