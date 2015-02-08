package ammonite.repl.eval
import acyclic.file
import ammonite.repl.Result
import ammonite.repl.eval.Preprocessor.Output

import scala.reflect.runtime._
import scala.util.Try


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

  def Processor(cond: tb.u.Tree => Boolean)
               (render: (String, String) => Preprocessor.Output) = {
    (code: String, name: String) => {
      if (cond(tb.parse(code))) Some(render(code, name))
      else None
    }
  }

  def getIdent(s: String) =
    s.takeWhile(!endOfIdentifier.contains(_))
      .trim()
      .split("\\s")
      .last


  def pprintSignature(ident: String) = s"""ReplBridge.shell.shellPPrint($ident, "$ident")"""

  def definedStr(name: String, code: String) = '"'+s"defined $name ${getIdent(code)}"+'"'

  /**
   * Sketchily used to find the name of the def/object/trait/class that
   * is being defined. Doesn't work in the general case, and at some point
   * should be replaced by proper use of parboiled
   */
  val endOfIdentifier = ":[(={"

  def pprint(ident: String) = {
    pprintSignature(ident) +
      s""" + " = " + ammonite.pprint.PPrint($ident)"""
  }
  def DefProcessor(cond: tb.u.Tree => Boolean, definitionLabel: String) =
    Processor(cond){ (code, name) =>
      Preprocessor.Output(code, definedStr(definitionLabel, getIdent(code)))
    }
  val ObjectDef = DefProcessor(_.isInstanceOf[tb.u.ModuleDef], "object")
  val ClassDef = DefProcessor(_.isInstanceOf[tb.u.ClassDef], "class")
//  val TraitDef = DefProcessor(_.isInstanceOf[tb.u.TraitDef], "trait")
  val DefDef = DefProcessor(_.isInstanceOf[tb.u.DefDef], "function")
  val TypeDef = DefProcessor(_.isInstanceOf[tb.u.TypeDef], "type")
  val PatVarDef = Processor(_.isInstanceOf[tb.u.ValDef]){ (code, name) =>
    Preprocessor.Output(
      code,
      if (!code.trim.startsWith("lazy")) pprint(getIdent(code))
      else pprintSignature(getIdent(code)) + s""" + " = <lazy>" """
    )
  }

  val Expr = Processor(_ => true){ (code, name) =>
    Preprocessor.Output(s"val $name = " + code, pprint(name))
  }
  val Import = Processor(_.isInstanceOf[tb.u.Import]){ (code, name) =>
    Preprocessor.Output(code, '"'+code+'"')
  }

  def decls(wrapperId: Int) = Seq[(String, String) => Option[Preprocessor.Output]](
    ObjectDef, ClassDef, /*TraitDef, */DefDef, TypeDef, PatVarDef, Import, Expr
  )

  def apply(code: String, wrapperId: Int): Result[Preprocessor.Output] = {
    val name = "res"+wrapperId

    util.Try(tb.parse(code)) match {
      case util.Failure(e) if e.getMessage.contains("expected but eof found") =>
        Result.Buffer(code)
      case util.Failure(e) =>
        Result.Failure(e.toString)
      case util.Success(parsed) if parsed.toString == "<empty>" =>
        Result.Failure("")
      case util.Success(parsed) =>
        def handleTree(t: tb.u.Tree, c: String, name: String) = {
          decls(wrapperId).iterator.flatMap(_.apply(c, name)).next()
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
