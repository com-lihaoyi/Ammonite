package ammonite.repl.interp
import acyclic.file
import ammonite.repl.Result
import ammonite.repl.interp.Preprocessor.Output
import org.parboiled2.ParseError

import scala.reflect.internal.Flags
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

  def apply(parse: => String => Either[String, Seq[Global#Tree]]): Preprocessor = new Preprocessor{
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
      val splitted = new scalaParser.Scala(code){
        def Split = {
          def Prelude = rule( Annot.* ~ `implicit`.? ~ `lazy`.? ~ LocalMod.* )
          rule( Semis.? ~ capture(Import | Prelude ~ BlockDef | StatCtx.Expr).*(Semis) ~ Semis.? ~ EOI )
        }
      }

      splitted.Split.run() match {
        case util.Failure(ParseError(p, pp, t)) if p.index == code.length => Result.Buffer(code)
        case util.Failure(e) => Result.Failure(parse(code).left.get)
        case util.Success(Nil) => Result.Skip
        case util.Success(postSplit: Seq[String]) =>

          val reParsed = postSplit.map(p => (parse(p), p))
          val errors = reParsed.collect{case (Left(e), _) => e }
          if (errors.length != 0) Result.Failure(errors.mkString("\n"))
          else {
            val allDecls = for (((Right(trees), code), i) <- reParsed.zipWithIndex) yield {
              // Suffix the name of the result variable with the index of
              // the tree if there is more than one statement in this command
              val suffix = if (reParsed.length > 1) "_" + i else ""
              def handleTree(t: Global#Tree) = {
                decls.iterator.flatMap(_.apply(code, "res" + wrapperId + suffix, t)).next()
              }
              trees match {
                // AFAIK this can only happen for pattern-matching multi-assignment,
                // which for some reason parse into a list of statements. In such a
                // scenario, aggregate all their printers, but only output the code once
                case Seq(tree) => handleTree(tree)
                case trees =>
                  val printers = for {
                    tree <- trees
                    if tree.isInstanceOf[Global#ValDef]
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
}

