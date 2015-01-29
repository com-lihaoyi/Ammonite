package ammonite.sh.eval
import acyclic.file
import ammonite.sh.eval.Preprocessor.Output
import org.parboiled2.ParserInput


class Recognizer(input: ParserInput) extends scalaParser.ScalaSyntax(input){

  def DefDef = rule( K.W("def") ~ FunDef )
  def ObjectDefX = rule( K.W("object") ~ ObjectDef )
  def ClassDefX = rule( K.W("class") ~ ClassDef )
  def TraitDefX = rule( K.W("trait") ~ TraitDef )
  def TypeDefX = rule( K.W("type") ~ TypeDef )
  def PatVarDefX = rule{ optional(K.W("lazy")) ~ PatVarDef }
}

object Preprocessor{
  case class Output(code: String, printer: String)

}
/**
 * Converts REPL-style snippets into full-fledged Scala source files,
 * ready to feed into the compiler. Each source-string is turned into
 * three things:
 */
class Preprocessor{
  def Processor(cond: Recognizer => util.Try[_])
               (render: (String, String) => Preprocessor.Output) = {
    (code: String, name: String) => {
      val rec = new Recognizer(code)
      if (cond(rec).isFailure) None
      else Some(rec.cursor -> render(code, name))
    }
  }

  def getIdent(s: String) =
    s.takeWhile(!endOfIdentifier.contains(_))
      .trim()
      .split("\\s")
      .last


  def pprintSignature(ident: String) = s"""
     Console.CYAN + "$ident" + Console.RESET + ": " +
     Console.GREEN + ammonite.sh.Shell.typeString($ident) + Console.RESET """

  def definedStr(name: String, code: String) = '"'+s"defined $name ${getIdent(code)}"+'"'

  /**
   * Sketchily used to find the name of the def/object/trait/class that
   * is being defined. Doesn't work in the general case, and at some point
   * should be replaced by proper use of parboiled
   */
  val endOfIdentifier = ":[(={"

  def pprint(ident: String) = {
    pprintSignature(ident) +
      s""" + " = " + ammonite.pprint.PPrint($ident) """
  }
  def DefProcessor(cond: Recognizer => util.Try[_], definitionLabel: String) =
    Processor(cond){ (code, name) =>
      Preprocessor.Output(code, definedStr(definitionLabel, getIdent(code)))
    }
  val ObjectDef = DefProcessor(_.ObjectDefX.run(), "object")
  val ClassDef = DefProcessor(_.ClassDefX.run(), "class")
  val TraitDef = DefProcessor(_.TraitDefX.run(), "trait")
  val DefDef = DefProcessor(_.DefDef.run(), "function")
  val TypeDef = DefProcessor(_.TypeDefX.run(), "type")
  val PatVarDef = Processor(_.PatVarDefX.run()){ (code, name) =>
    Preprocessor.Output(
      code,
      if (!code.trim.startsWith("lazy")) pprint(getIdent(code))
      else pprintSignature(getIdent(code)) + s""" + " = <lazy>" """
    )
  }

  val Expr = Processor(_.Expr.run()){ (code, name) =>
    Preprocessor.Output(s"val $name = " + code, pprint(name))
  }
  val Import = Processor(_.Import.run()){ (code, name) =>
    Preprocessor.Output(code, code)
  }

  def decls(wrapperId: Int) = Seq[(String, String) => Option[(Int, Preprocessor.Output)]](
    ObjectDef, ClassDef, TraitDef, DefDef, TypeDef, PatVarDef, Expr, Import
  )

  def apply(code: String, wrapperId: Int): Option[Preprocessor.Output] = {
    val name = "res"+wrapperId
    def rec(current: String): List[Output] = {
      if(current == "") Nil
      else {
        val (offset, output) = decls(wrapperId).iterator.flatMap(_.apply(current, name)).next()
        output :: rec(current.drop(offset))
      }
    }
    val allDecls = rec(code)
    allDecls.reduceOption((a, b) => Output(a.code+b.code, a.printer+b.printer))
  }
}
