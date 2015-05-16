package ammonite.repl.interp
import acyclic.file
import ammonite.repl.{BacktickWrap, Res}

import scala.reflect.internal.Flags
import scala.tools.nsc.{Global => G}

/**
 * Converts REPL-style snippets into full-fledged Scala source files,
 * ready to feed into the compiler. Each source-string is turned into
 * three things:
 */
trait Preprocessor{
  def apply(code: String, wrapperId: String): Res[Preprocessor.Output]
}
object Preprocessor{

  case class Output(code: String, printer: Seq[String])

  def apply(parse: => String => Either[String, Seq[G#Tree]]): Preprocessor = new Preprocessor{

    def Processor(cond: PartialFunction[(String, String, G#Tree), Preprocessor.Output]) = {
      (code: String, name: String, tree: G#Tree) => cond.lift(name, code, tree)
    }

    def pprintSignature(ident: String) = s"""Iterator(ReplBridge.shell.shellPPrint($ident, "$ident"))"""

    def definedStr(definitionLabel: String, name: String) =
      s"""Iterator(ReplBridge.shell.shellPrintDef("$definitionLabel", "$name"))"""

    def pprint(ident: String) = {
      pprintSignature(ident) +
        s""" ++ Iterator(" = ") ++ ammonite.pprint.PPrint($ident)"""
    }

    /**
     * Processors for declarations which all have the same shape
     */
    def DefProc(definitionLabel: String)(cond: PartialFunction[G#Tree, G#Name]) =
      (code: String, name: String, tree: G#Tree) =>
        cond.lift(tree).map{ name =>
          Preprocessor.Output(
            code,
            Seq(definedStr(definitionLabel, BacktickWrap(name.decoded)))
          )
        }

    val ObjectDef = DefProc("object"){case m: G#ModuleDef => m.name}
    val ClassDef = DefProc("class"){ case m: G#ClassDef if !m.mods.isTrait => m.name }
    val TraitDef =  DefProc("trait"){ case m: G#ClassDef if m.mods.isTrait => m.name }
    val DefDef = DefProc("function"){ case m: G#DefDef => m.name }
    val TypeDef = DefProc("type"){ case m: G#TypeDef => m.name }

    val PatVarDef = Processor { case (name, code, t: G#ValDef) =>
      //Function to lift lhs expressions into anonymous functions so they will be JITed
      def wrap(code: String)={
        import fastparse._
        import scalaparse.Scala._
        val par = P( ( `implicit`.? ~ `lazy`.? ~ ( `var` | `val` ) ~! BindPattern.rep(1, "," ~! Pass) ~ (`:` ~! Type).?).! ~ (`=` ~! StatCtx.Expr.!) )
        val Result.Success((lhs, rhs), _) = par.parse(code)
        //Rebuilding definition from parsed data to lift rhs to anon function
        s"$lhs = { () =>\n $rhs \n}.apply"
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
        else if (!t.mods.hasFlag(Flags.LAZY)) Seq(pprint(BacktickWrap.apply(t.name.decoded)))
        else Seq(s"""${pprintSignature(BacktickWrap.apply(t.name.decoded))} ++ Iterator(" = <lazy>")""")
      )
    }

    val Import = Processor{
      case (name, code, tree: G#Import) =>
        val Array(keyword, body) = code.split(" ", 2)
        Output(code, Seq(s"""Iterator(ReplBridge.shell.shellPrintImport("$body"))"""))
    }

    val Expr = Processor{
      //Expressions are lifted to anon function applications so they will be JITed
      case (name, code, tree) => Output(s"val $name = { () =>\n$code\n}.apply", Seq(pprint(name)))
    }

    val decls = Seq[(String, String, G#Tree) => Option[Preprocessor.Output]](
      ObjectDef, ClassDef, TraitDef, DefDef, TypeDef, PatVarDef, Import, Expr
    )

    def apply(code: String, wrapperId: String): Res[Preprocessor.Output] = {
      import fastparse._
      import scalaparse.Scala._
      val Prelude = P( Annot.rep ~ `implicit`.? ~ `lazy`.? ~ LocalMod.rep )
      val Splitter = P( Semis.? ~ (scalaparse.Scala.Import | Prelude ~ BlockDef | StatCtx.Expr).!.rep(sep=Semis) ~ Semis.? ~ WL ~ End)


      Splitter.parse(code) match {
        case Result.Failure(_, index) if index == code.length => Res.Buffer(code)
        case f @ Result.Failure(p, index) =>

          Res.Failure(parse(code).left.get)
        case Result.Success(Nil, _) => Res.Skip
        case Result.Success(postSplit: Seq[String], _) => complete(code, wrapperId, postSplit.map(_.trim))
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

