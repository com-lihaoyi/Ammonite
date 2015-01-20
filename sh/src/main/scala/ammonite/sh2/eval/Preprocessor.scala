package ammonite.sh2.eval
import acyclic.file
import org.parboiled2.ParserInput


class Recognizer(input: ParserInput) extends scalaParser.ScalaSyntax(input){

  def DefDef = rule( K.W("def") ~ FunDef )
  def ObjectDefX = rule( K.W("object") ~ ObjectDef )
  def ClassDefX = rule( K.W("class") ~ ClassDef )
  def TraitDefX = rule( K.W("trait") ~ TraitDef )
  def PatVarDefX = rule{ optional(K.W("lazy")) ~ PatVarDef }
}





class Preprocessor{
  def Processor(cond: Recognizer => util.Try[_])(render: (String, String) => (String, String)) = {
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
     Console.GREEN + ammonite.sh2.Shell2.typeString($ident) + Console.RESET """

  def definedStr(name: String, code: String) = '"'+s"defined $name ${getIdent(code)}"+'"'

  val endOfIdentifier = ":[(="

  def pprint(ident: String) = {
    pprintSignature(ident) +
      s""" + " = " + ammonite.sh2.PPrint($ident) """
  }
  val ObjectDef = Processor(_.ObjectDefX.run()){ (name, code) =>
    s"import $$$name._" -> standardBlob(name, code, definedStr("object", name))
  }
  val ClassDef = Processor(_.ClassDefX.run()){ (name, code) =>
    s"import $$$name._" -> standardBlob(name, code, definedStr("class", name))
  }
  val TraitDef = Processor(_.TraitDefX.run()){ (name, code) =>
    s"import $$$name._" -> standardBlob(name, code, definedStr("trait", name))
  }
  val DefDef = Processor(_.DefDef.run()){ (name, code) =>
    s"import $$$name._" -> standardBlob(name, code, definedStr("function", getIdent(code)))
  }
  val PatVarDef = Processor(_.PatVarDefX.run()){ (name, code) =>
    s"import $$$name._" -> standardBlob(name, code,
      if (!code.trim.startsWith("lazy")) pprint(getIdent(code))
      else pprintSignature(getIdent(code)) + s""" + " = <lazy>" """
    )
  }
  val Expr = Processor(_.Expr.run()){ (name, code) =>
    s"import $$$name._" -> standardBlob(name, s"val $name = " + code, pprint(name))
  }
  val Import = Processor(_.Import.run()){ (name, code) =>
    code -> s"""object $$$name{def $$main() = "$code"}"""
  }

  def decls(wrapperId: Int) = Seq[(String, String) => Option[(String, String)]](
    ObjectDef, ClassDef, TraitDef, DefDef, PatVarDef, Expr, Import
  )

  def apply(code: String, wrapperId: Int): Option[(String, String, String)] = {
    val name = "res"+wrapperId
    decls(wrapperId)
      .iterator
      .map(_(name, code))
      .reduce(_ orElse _)
      .map{case (imports, wrappedCode) => (imports, wrappedCode, name)}
  }
}
