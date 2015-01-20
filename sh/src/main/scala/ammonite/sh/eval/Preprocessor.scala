package ammonite.sh.eval
import acyclic.file
import org.parboiled2.ParserInput


class Recognizer(input: ParserInput) extends scalaParser.ScalaSyntax(input){

  def DefDef = rule( K.W("def") ~ FunDef )
  def ObjectDefX = rule( K.W("object") ~ ObjectDef )
  def ClassDefX = rule( K.W("class") ~ ClassDef )
  def TraitDefX = rule( K.W("trait") ~ TraitDef )
  def PatVarDefX = rule{ optional(K.W("lazy")) ~ PatVarDef }
}

object Preprocessor{
  type Output = (String, String, String)
}
/**
 * Converts REPL-style snippets into full-fledged Scala source files,
 * ready to feed into the compiler. Each source-string is turned into
 * three things:
 */
class Preprocessor{
  def Processor(cond: Recognizer => util.Try[_])
               (render: (String, String) => (String, String, String)) = {
    (name: String, code: String) => {
      if (cond(new Recognizer(code)).isFailure) None
      else Some(render(name, code))
    }
  }
  def standardBlob(name: String, code: String, printer: String): String = s"""
    object $$$name{
      $code
      def $$main() = {$printer}
    }
  """
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
      s""" + " = " + ammonite.sh.PPrint($ident) """
  }
  def DefProcessor(cond: Recognizer => util.Try[_], definitionLabel: String) =
    Processor(cond){ (name, code) => (
      getIdent(code),
      s"import $$$name._",
      standardBlob(name, code, definedStr(definitionLabel, getIdent(code)))
    )}
  val ObjectDef = DefProcessor(_.ObjectDefX.run(), "object")
  val ClassDef = DefProcessor(_.ClassDefX.run(), "class")
  val TraitDef = DefProcessor(_.TraitDefX.run(), "trait")
  val DefDef = DefProcessor(_.DefDef.run(), "function")
  val PatVarDef = Processor(_.PatVarDefX.run()){ (name, code) =>
    (getIdent(code), s"import $$$name._", standardBlob(name, code,
      if (!code.trim.startsWith("lazy")) pprint(getIdent(code))
      else pprintSignature(getIdent(code)) + s""" + " = <lazy>" """
    ))
  }
  val Expr = Processor(_.Expr.run()){ (name, code) =>
    (name, s"import $$$name._", standardBlob(name, s"val $name = " + code, pprint(name)))
  }
  val Import = Processor(_.Import.run()){ (name, code) =>
    (code, code, s"""object $$$name{def $$main() = "$code"}""")
  }

  def decls(wrapperId: Int) = Seq[(String, String) => Option[(String, String, String)]](
    ObjectDef, ClassDef, TraitDef, DefDef, PatVarDef, Expr, Import
  )

  def apply(code: String, wrapperId: Int): Option[Preprocessor.Output] = {
    val name = "res"+wrapperId
    decls(wrapperId)
      .iterator
      .map(_(name, code))
      .reduce(_ orElse _)
  }
}
