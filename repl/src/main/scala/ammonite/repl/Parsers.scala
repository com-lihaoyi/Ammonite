package ammonite.repl

import fastparse.all._

object Parsers {

  import fastparse.noApi._

  import scalaparse.Scala._
  import WhitespaceApi._

  val PatVarSplitter = {
    val Prefixes = P(Prelude ~ (`var` | `val`))
    val Lhs = P( Prefixes ~! BindPattern.rep(1, "," ~! Pass) ~ (`:` ~! Type).? )
    P( Lhs.! ~ (`=` ~! WL ~ StatCtx.Expr.!) ~ End )
  }
  def patVarSplit(code: String) = {
    val Result.Success((lhs, rhs), _) = PatVarSplitter.parse(code)
    (lhs, rhs)
  }
  val Id2 = P( Id ~ End )
  def backtickWrap(s: String) = {
    Id2.parse(s) match{
      case _: Result.Success[_] => s
      case _ => "`" + pprint.PPrinter.escape(s) + "`"
    }
  }
  val Prelude = P( (Annot ~ OneNLMax).rep ~ (Mod ~! Pass).rep )
  val Statement = P ( scalaparse.Scala.TopPkgSeq | scalaparse.Scala.Import | Prelude ~ BlockDef | StatCtx.Expr )
  def StatementBlock(blockSep: P0) = P ( Semis.? ~ (!blockSep ~ Statement).!.repX(sep=Semis) ~ Semis.? )
  val Splitter = P( StatementBlock(Fail) ~ WL ~ End)

  /**
   * Attempts to break a code blob into multiple statements. Returns `None` if
   * it thinks the code blob is "incomplete" and requires more input
   */
  def split(code: String): Option[fastparse.core.Result[Seq[String]]] = Splitter.parse(code) match{
    case Result.Failure(_, index) if code.drop(index).trim() == "" => None
    case x => Some(x)
  }

  val Separator = P( WL ~ "@" ~~ CharIn(" \n").rep(1) )
  val CompilationUnit = P( WL ~ StatementBlock(Separator) ~ WL )
  val ScriptSplitter = P( CompilationUnit.repX(1, Separator) ~ End)
  def splitScript(code: String) = ScriptSplitter.parse(code).get.value

  val BlockUnwrapper = P( "{" ~ Block.! ~ "}" ~ End)
  def unwrapBlock(code: String) = {
    BlockUnwrapper.parse(code) match{
      case Result.Success(contents, _) => Some(contents)
      case _ => None
    }
  }

  object PathComplete{

    def RevSymbolBase(i: Int) = P( WS ~ (!"/") ~ scalaparse.syntax.Basic.LetterDigitDollarUnderscore.rep(i).!.map(_.reverse) ~ "'" )
    val RevSymbol0 = P( RevSymbolBase(0) )
    val RevSymbol = P( RevSymbolBase(1) )
    def SingleChars = P( (!"\"" ~ AnyChar | "\"\\" ).rep )
    val RevString0 = P( WS ~ (!"/") ~ SingleChars.!.map(_.reverse.replace("\\\"", "\"").replace("\\\\", "\\")) ~ "\"" )
    val RevString = P( "\"" ~! RevString0 )

    val Head = P( (RevString0 | RevSymbol0).? )
    val Body = P( ("/" ~ (RevSymbol | RevString)).rep.map(_.reverse) )
    val Tail = P( ("/" ~ scalaparse.syntax.Identifiers.Id.!.map(_.reverse)).? )
    val RevPath = P( (Head ~~ Index ~ Body ~ Tail).map{case (a, i, b, c) => (c, b, a, i)} )

    def stringSymWrap(s: String) = {
      if (s == "") "'"
      else (scalaparse.syntax.Identifiers.Id ~ End).parse(s, 0)  match{
        case Result.Success(v, _) =>  "'" + s
        case f: Result.Failure => "\"" + pprint.PPrinter.escape(s) + "\""
      }
    }
  }
}
