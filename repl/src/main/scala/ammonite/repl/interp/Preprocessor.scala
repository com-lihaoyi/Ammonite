package ammonite.repl.interp
import acyclic.file
import ammonite.repl.{Parsers, Res}

import scala.reflect.internal.Flags
import scala.tools.nsc.{Global => G}

/**
 * Converts REPL-style snippets into full-fledged Scala source files,
 * ready to feed into the compiler. Each source-string is turned into
 * three things:
 */
trait Preprocessor{
  def apply(stmts: Seq[String], wrapperId: String): Res[Preprocessor.Output]
}
object Preprocessor{

  case class Output(code: String, printer: Seq[String])

  def apply(parse: => String => Either[String, Seq[G#Tree]]): Preprocessor = new Preprocessor{

    def Processor(cond: PartialFunction[(String, String, G#Tree), Preprocessor.Output]) = {
      (code: String, name: String, tree: G#Tree) => cond.lift(name, code, tree)
    }

    def pprintSignature(ident: String, customMsg: Option[String]) = {
      val customCode = customMsg.fold("None")(x => s"""Some("$x")""")
      s"""ReplBridge.shell.Internal.print($ident, "$ident", $customCode)"""
    }
    def definedStr(definitionLabel: String, name: String) =
      s"""ReplBridge.shell.Internal.printDef("$definitionLabel", "$name")"""

    def pprint(ident: String) = pprintSignature(ident, None)


    /**
     * Processors for declarations which all have the same shape
     */
    def DefProc(definitionLabel: String)(cond: PartialFunction[G#Tree, G#Name]) =
      (code: String, name: String, tree: G#Tree) =>
        cond.lift(tree).map{ name =>
          Preprocessor.Output(
            code,
            Seq(definedStr(definitionLabel, Parsers.backtickWrap(name.decoded)))
          )
        }

    val ObjectDef = DefProc("object"){case m: G#ModuleDef => m.name}
    val ClassDef = DefProc("class"){ case m: G#ClassDef if !m.mods.isTrait => m.name }
    val TraitDef =  DefProc("trait"){ case m: G#ClassDef if m.mods.isTrait => m.name }
    val DefDef = DefProc("function"){ case m: G#DefDef => m.name }
    val TypeDef = DefProc("type"){ case m: G#TypeDef => m.name }

    val PatVarDef = Processor { case (name, code, t: G#ValDef) =>
      //Function to RHS expressions into anonymous functions so they will be JITed
      def wrap(code: String) = {
        val (lhs, rhs) = Parsers.patVarSplit(code)
        //Rebuilding definition from parsed data to lift rhs to anon function
        s"$lhs = { () =>\n$rhs \n}.apply\n"
      }

      Output(
        //Only wrap rhs in function if it is not a function
        //Wrapping functions causes type inference errors.
        t.rhs match {
          case _: G#Function => code //simple anon function
          case _: G#Match => code   //anon partial function
          case _ => wrap(code)
        },
        // Try to leave out all synthetics; we don't actually have proper
        // synthetic flags right now, because we're dumb-parsing it and not putting
        // it through a full compilation
        if (t.name.decoded.contains("$")) Nil
        else if (!t.mods.hasFlag(Flags.LAZY)) Seq(pprint(Parsers.backtickWrap(t.name.decoded)))
        else Seq(s"""${pprintSignature(Parsers.backtickWrap(t.name.decoded), Some("<lazy>"))}""")
      )
    }

    val Import = Processor{
      case (name, code, tree: G#Import) =>
        val Array(keyword, body) = code.split(" ", 2)
        Output(code, Seq(s"""ReplBridge.shell.Internal.printImport("$body")"""))
    }

    val Expr = Processor{
      //Expressions are lifted to anon function applications so they will be JITed
      case (name, code, tree) => Output(s"val $name = { () =>\n$code\n}.apply\n", Seq(pprint(name)))
    }

    val decls = Seq[(String, String, G#Tree) => Option[Preprocessor.Output]](
      ObjectDef, ClassDef, TraitDef, DefDef, TypeDef, PatVarDef, Import, Expr
    )

    def apply(stmts: Seq[String], wrapperId: String): Res[Preprocessor.Output] = {
      val unwrapped = stmts.flatMap{x => Parsers.unwrapBlock(x) match {
        case Some(contents) => Parsers.split(contents)
        case None => Seq(x)
      }}
      unwrapped match{
        case Nil => Res.Skip
        case postSplit => complete(stmts.mkString, wrapperId, postSplit.map(_.trim))
      }
    }
    
    def complete(code: String, wrapperId: String, postSplit: Seq[String]) = {
      val reParsed = postSplit.map(p => (parse(p), p))
      val errors = reParsed.collect{case (Left(e), _) => e }
      if (errors.length != 0) Res.Failure(errors.mkString("\n"))
      else {
        val allDecls = for (((Right(trees), code), i) <- reParsed.zipWithIndex) yield {
          // Suffix the name of the result variable with the index of
          // the tree if there is more than one statement in this command
          val suffix = if (reParsed.length > 1) "_" + i else ""
          def handleTree(t: G#Tree) = {
            decls.iterator.flatMap(_.apply(code, "res" + wrapperId + suffix, t)).next()
          }
          trees match {
            case Seq(tree) => handleTree(tree)

            // This handles the multi-import case `import a.b, c.d`
            case trees if trees.forall(_.isInstanceOf[G#Import]) => handleTree(trees(0))

            // AFAIK this can only happen for pattern-matching multi-assignment,
            // which for some reason parse into a list of statements. In such a
            // scenario, aggregate all their printers, but only output the code once
            case trees =>
              val printers = for {
                tree <- trees
                if tree.isInstanceOf[G#ValDef]
                Preprocessor.Output(_, printers) = handleTree(tree)
                printer <- printers
              } yield printer
              Preprocessor.Output(code, printers)
          }
        }
        Res(
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

