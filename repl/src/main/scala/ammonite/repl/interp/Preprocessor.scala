package ammonite.repl.interp
import acyclic.file
import ammonite.repl.{Parsed, Result}
import ammonite.repl.interp.Preprocessor.Output
import scala.reflect.internal.Flags
import scala.reflect.internal.util.NoPosition
import scala.tools.nsc.Global


/**
 * Converts REPL-style snippets into full-fledged Scala source files,
 * ready to feed into the compiler. Each source-string is turned into
 * three things:
 */
trait Preprocessor{
  def apply(code: String, wrapperId: Int): Result[Preprocessor.Output]
}
object Preprocessor{

  case class Output(code: String, printer: Seq[String])

  class ScalaParserX(x: org.parboiled2.ParserInput) extends scalaParser.Scala(x){
    def Body = rule( `=` ~ `macro`.? ~ StatCtx.Expr | OneNLMax ~ '{' ~ Block ~ "}" )
    def FunDef = rule( `def` ~ FunSig ~ (`:` ~ Type).? ~ Body )
    def DefId = rule(
      `object` | `def` | `type`
    )
  }
  def apply(parse: => String => Parsed): Preprocessor = new Preprocessor{
    def Processor(cond: PartialFunction[(String, String, Global#Tree), Preprocessor.Output]) = {
      (code: String, name: String, tree: Global#Tree) => cond.lift(name, code, tree)
    }

    def pprintSignature(ident: String) = s"""Iterator(ReplBridge.shell.shellPPrint(`$ident`, "$ident"))"""

    def definedStr(definitionLabel: String, name: String) =
      s"""Iterator(ReplBridge.shell.shellPrintDef("$definitionLabel", "$name"))"""

    def pprint(ident: String) = {
      pprintSignature(ident) +
        s""" ++ Iterator(" = ") ++ ammonite.pprint.PPrint(`$ident`)"""
    }
    def DefProcessor(definitionLabel: String)(cond: PartialFunction[Global#Tree, String]) =
      (code: String, name: String, tree: Global#Tree) =>
        cond.lift(tree).map{
          name => Preprocessor.Output(code, Seq(definedStr(definitionLabel, name)))
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
        // Try to leave out all synthetics; we don't actually have proper
        // synthetic flags right now, because we're dumb-parsing it and not putting
        // it through a full compilation
        if (t.name.decoded.contains("$")) Nil
        else if (!t.mods.hasFlag(Flags.LAZY)) Seq(pprint(t.name.toString))
        else Seq(s"""${pprintSignature(t.name.toString)} ++ Iterator(" = <lazy>")""")
      )
    }

    val Expr = Processor{ case (name, code, tree) =>
      Preprocessor.Output(s"val $name = ($code)", Seq(pprint(name)))
    }
    val Import = Processor{ case (name, code, tree: Global#Import) =>
      Preprocessor.Output(code, Seq(s"""Iterator("$code")"""))
    }

    val decls = Seq[(String, String, Global#Tree) => Option[Preprocessor.Output]](
      ObjectDef, ClassDef, TraitDef, DefDef, TypeDef, PatVarDef, Import, Expr
    )

    def apply(code: String, wrapperId: Int): Result[Preprocessor.Output] = {

      parse(code) match {
        case Parsed.Incomplete => Result.Buffer(code)
        case Parsed.Error(msg) => Result.Failure(msg)
        case Parsed.Success(Nil) => Result.Skip
        case Parsed.Success(_) =>

          val splitted = new scalaParser.Scala(code){
            def Split = {
              def Prelude = rule( Annot.* ~ `implicit`.? ~ `lazy`.? ~ LocalMod.* )
              rule( capture(Import | Prelude ~ BlockDef | StatCtx.Expr).+(Semis) )
            }
          }
          val postSplit: Seq[String] = splitted.Split.run().get
          val zipped = postSplit.map(p => (parse(p), p)).zipWithIndex

          val allDecls = for (((Parsed.Success(trees), code), i) <- zipped) yield {
            // Suffix the name of the result variable with the index of
            // the tree if there is more than one statement in this command
            val suffix = if(zipped.length > 1) "_" + i else ""
            def handleTree(t: Global#Tree) = {
              decls.iterator.flatMap(_.apply(code, "res" + wrapperId + suffix, t)).next()
            }
            trees match{
              case Seq(tree) => handleTree(tree)
              // AFAIK this can only happen for pattern-matching multi-assignment,
              // which for some reason parse into a list of statements. In such a
              // scenario, aggregate all their printers, but only output the code once
              case trees =>
                val printers = for{
                  tree <- trees
                  Preprocessor.Output(_, printers) = handleTree(tree)
                  printer <- printers
                } yield printer
                Preprocessor.Output(code, printers)
            }
          }

          Result(
            allDecls.reduceOption { (a, b) =>
              Output(
                a.code + ";" + b.code,
                a.printer ++ b.printer
              )
            },
            "Don't know how to handle " + code
          )
      }
    }
  }
}

