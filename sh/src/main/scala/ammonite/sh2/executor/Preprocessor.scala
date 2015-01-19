package ammonite.sh2.executor

import org.parboiled2.ParserInput


class Recognizer(input: ParserInput) extends scalaParser.ScalaSyntax(input){

  def DefDef = rule( K.W("def") ~ FunDef )
  def ObjectDefX = rule( K.W("object") ~ ObjectDef )
  def ClassDefX = rule( K.W("class") ~ ClassDef )
  def TraitDefX = rule( K.W("trait") ~ TraitDef )
  def PatVarDefX = rule{ optional(K.W("lazy")) ~ PatVarDef }
}

object Preprocessor {
  val endOfIdentifier = ":[(="
  case class Processor(tag: String,
                       condition: Recognizer => util.Try[_],
                       pref: () => String = ()=>"",
                       printer: (String) => String = s=>"")
  def pprintSignature(ident: String) = {
    s"""
         Console.CYAN +
         "$ident" +
         Console.RESET +
         ": " +
         Console.GREEN +
         ammonite.sh2.Shell2.typeString($ident) +
         Console.RESET
    """
  }
  def pprint(ident: String) = {
    pprintSignature(ident) +
      s"""
       + " = " +
       ammonite.sh2.PPrint($ident)
      """
  }
  def getIdent(s: String) =
    s.takeWhile(!endOfIdentifier.contains(_))
      .trim()
      .split("\\s")
      .last

  val decls = Seq[Processor](
    Processor("1", _.ObjectDefX.run(), printer = s => s""" "defined object ${getIdent(s)}" """),
    Processor("2", _.ClassDefX.run(), printer = s => s""" "defined class ${getIdent(s)}" """),
    Processor("3", _.TraitDefX.run(), printer = s => s""" "defined trait ${getIdent(s)}" """),
    Processor("4", _.PatVarDefX.run(), printer = s => {
      if (!s.trim.startsWith("lazy"))
        pprint(getIdent(s))
      else
        pprintSignature(getIdent(s)) + s""" + " = <lazy> " """
    }),
    Processor("5", _.DefDef.run(), printer = s => pprintSignature(getIdent(s))),
    Processor("6", _.TypeDef.run()),
    Processor("7", _.Import.run()),
    Processor(
      "8",
      _.Expr.run(),
      () => {id += 1; s"val res$id = "},
      s => pprint(s"res$id")
    )
  )
  def apply(code: String): Option[(String, String)] = {


    for(proc <- decls.find(_.condition(new Recognizer(code)).isSuccess)) yield {
      //      println(proc.tag)
      wrapperId += 1
      val imports = for(i <- 1 until wrapperId) yield s"import $$ammWrap$i._"
      val txt =
        s"""
          ${imports.mkString("\n")}
          object $$ammWrap$wrapperId{
            ${proc.pref()} $code
            def $$main() = {println(${proc.printer(code)})}
          }
        """
      (s"$$ammWrap$wrapperId",  txt)
    }
  }

  var wrapperId = 0
  var id = 0
}
