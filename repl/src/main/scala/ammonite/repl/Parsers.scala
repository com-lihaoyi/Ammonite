package ammonite.repl

import fastparse.all._

object Parsers {

  import fastparse.noApi._

  import scalaparse.Scala._
  import WhitespaceApi._

  val PatVarSplitter = {
    val Prefixes = P(Prelude ~ (`var` | `val`))
    val Lhs = P( Prefixes ~/ BindPattern.rep(1, "," ~/ Pass) ~ (`:` ~/ Type).? )
    P( Lhs.! ~ (`=` ~/ WL ~ StatCtx.Expr.!) ~ End )
  }
  def patVarSplit(code: String) = {
    val Parsed.Success((lhs, rhs), _) = PatVarSplitter.parse(code)
    (lhs, rhs)
  }
  val Id2 = P( Id ~ End )
  def backtickWrap(s: String) = {
    Id2.parse(s) match{
      case _: Parsed.Success[_] => s
      case _ => "`" + pprint.PPrinter.escape(s) + "`"
    }
  }
  val Prelude = P( (Annot ~ OneNLMax).rep ~ (Mod ~/ Pass).rep )
  val Statement =
    P( scalaparse.Scala.TopPkgSeq | scalaparse.Scala.Import | Prelude ~ BlockDef | StatCtx.Expr )
  def StatementBlock(blockSep: P0) =
    P( Semis.? ~ (!blockSep ~ Statement).!.repX(sep=Semis) ~ Semis.? )
  val Splitter = P( StatementBlock(Fail) ~ WL ~ End)

  /**
   * Attempts to break a code blob into multiple statements. Returns `None` if
   * it thinks the code blob is "incomplete" and requires more input
   */
  def split(code: String): Option[fastparse.core.Parsed[Seq[String]]] = Splitter.parse(code) match{
    case Parsed.Failure(_, index, extra) if code.drop(index).trim() == "" => None
    case x => Some(x)
  }

  val Separator = P( WL ~ "@" ~~ CharIn(" \n").rep(1) )
  val CompilationUnit = P( WL ~ StatementBlock(Separator) ~ WL )
  val ScriptSplitter = P( CompilationUnit.repX(1, Separator) ~ End)
  def splitScript(code: String) = ScriptSplitter.parse(code)

  val BlockUnwrapper = P( "{" ~ Block.! ~ "}" ~ End)
  def unwrapBlock(code: String) = {
    BlockUnwrapper.parse(code) match{
      case Parsed.Success(contents, _) => Some(contents)
      case _ => None
    }
  }

  def stringWrap(s: String) = "\"" + pprint.PPrinter.escape(s) + "\""
  def stringSymWrap(s: String) = {
    if (s == "") "'"
    else (scalaparse.syntax.Identifiers.Id ~ End).parse(s, 0)  match{
      case Parsed.Success(v, _) =>  "'" + s
      case f: Parsed.Failure => stringWrap(s)
    }
  }
}
