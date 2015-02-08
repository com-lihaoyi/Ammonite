package ammonite.repl.eval
import acyclic.file
import ammonite.repl.{Parsed, Result}
import ammonite.repl.eval.Preprocessor.Output
import scala.reflect.internal.Flags
import scala.reflect.internal.util.NoPosition
import scala.tools.nsc.Global

object Preprocessor{
  case class Output(code: String, printer: String)
}

/**
 * Converts REPL-style snippets into full-fledged Scala source files,
 * ready to feed into the compiler. Each source-string is turned into
 * three things:
 */
class Preprocessor(parse: String => Parsed){
  

  def Processor(cond: PartialFunction[(String, String, Global#Tree), Preprocessor.Output]) = {
    (code: String, name: String, tree: Global#Tree) => cond.lift(name, code, tree)
  }

  def pprintSignature(ident: String) = s"""ReplBridge.shell.shellPPrint($ident, "$ident")"""

  def definedStr(definitionLabel: String, name: String) =
    s"""ReplBridge.shell.shellPrintDef("$definitionLabel", "$name") """

  def pprint(ident: String) = {
    pprintSignature(ident) +
      s""" + " = " + ammonite.pprint.PPrint($ident)"""
  }
  def DefProcessor(definitionLabel: String)(cond: PartialFunction[Global#Tree, String]) =
    (code: String, name: String, tree: Global#Tree) =>
      cond.lift(tree).map{
        name => Preprocessor.Output(code, definedStr(definitionLabel, name))
      }

  val ObjectDef = DefProcessor("object"){case m: Global#ModuleDef => m.name.toString}
  val ClassDef = DefProcessor("class"){
    case m: Global#ClassDef if !m.mods.hasFlag(Flags.TRAIT)=> m.name.toString
  }
  val TraitDef =  DefProcessor("trait"){
    case m: Global#ClassDef if m.mods.hasFlag(Flags.TRAIT) => m.name.toString
  }

  val DefDef = DefProcessor("function"){case m: Global#DefDef => m.name.toString}
  val TypeDef = DefProcessor("type"){case m: Global#TypeDef => m.name.toString}

  val PatVarDef = Processor { case (name, code, t: Global#ValDef) =>
    Preprocessor.Output(
      code,
      if (!t.mods.hasFlag(Flags.LAZY)) pprint(t.name.toString)
      else pprintSignature(t.name.toString) + s""" + " = <lazy>" """
    )
  }

  val Expr = Processor{ case (name, code, tree) =>
    Preprocessor.Output(s"val $name = ($code)", pprint(name))
  }
  val Import = Processor{ case (name, code, tree: Global#Import) =>
    Preprocessor.Output(code, '"'+code+'"')
  }

  val decls = Seq[(String, String, Global#Tree) => Option[Preprocessor.Output]](
    ObjectDef, ClassDef, TraitDef, DefDef, TypeDef, PatVarDef, Import, Expr
  )

  def apply(code: String, wrapperId: Int): Result[Preprocessor.Output] = {
    parse(code) match {
      case Parsed.Incomplete => Result.Buffer(code)
      case Parsed.Error => Result.Skip
      case Parsed.Success(Nil) => Result.Skip
      case Parsed.Success(parsed) =>

        def handleTree(t: Global#Tree, c: String, name: String) = {
          decls.iterator.flatMap(_.apply(c, name, t)).next()
        }
        val zipped = parsed.zipWithIndex
        val allDecls = for((tree, i) <- zipped) yield {
          // _.pos.start doesn't work, we need to recurse into the trees
          // to find the position we want, which _.collect does for us
          val positions = tree.collect{case x => x.pos}.filter(_ != NoPosition)
          val start = positions.map(_.start).min
          val end = positions.map(_.end).max
          val suffix = if(parsed.length > 1) "_" + i else ""
          handleTree(tree, code.substring(start, end), "res" + wrapperId + suffix)
        }

        Result(
          allDecls.reduceOption((a, b) =>
            Output(a.code+";"+b.code, a.printer+ "+'\n'+" + b.printer)
          ),
          "Don't know how to handle " + code
        )
      }
  }
}
