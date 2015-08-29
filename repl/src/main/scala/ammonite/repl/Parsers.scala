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
  val Statement =
    P( scalaparse.Scala.TopPkgSeq | scalaparse.Scala.Import | Prelude ~ BlockDef | StatCtx.Expr )
  def StatementBlock(blockSep: P0) =
    P( Semis.? ~ (!blockSep ~ Statement).!.repX(sep=Semis) ~ Semis.? )
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

  /**
   * An ad-hoc parser that's meant to parse *backwards* from the location of
   * the current cursor, and identify whether there is a path-looking prefix
   * leading up to that location. This basically comprises reversed versions of
   * Symbol and String parsers, as well as "partial" reversed versions that
   * kick in if you want autocomplete halfway-through typeing a symbol/string.
   */
  object PathComplete{

    import scalaparse.syntax.Basic.LetterDigitDollarUnderscore
    def BaseRevSymbol(p: P[_]) = P( WS ~ (!"/") ~ p.!.map(_.reverse) )

    def SingleChars = P( (!"\"" ~ AnyChar | "\"\\" ).rep )
    def reverseNormalize(s: String) = {
      s.reverse.replace("\\\"", "\"").replace("\\\\", "\\")
    }
    def BaseRevString(f: String => String) = P( WS ~ (!"/") ~ SingleChars.!.map(f) ~ "\"" )

    val Head = {
      val RevSymbol0 = P( BaseRevSymbol(LetterDigitDollarUnderscore.rep(0) ~ "'") )
      val RevString0 = P( BaseRevString("\"" + reverseNormalize(_)) )
      P( (RevString0 | RevSymbol0).? )
    }

    val Segment = {
      val RevString1 = P( "\"" ~! BaseRevString(reverseNormalize) )
      val RevSymbol1 = P( BaseRevSymbol(LetterDigitDollarUnderscore.rep(1)) ~ "'" )
      P( RevSymbol1.map(Some(_)) | RevString1.map(Some(_)) | SegmentIdent.map(_ => None) )
    }

    val Body = P( ("/" ~ Segment).rep.map(_.reverse) )
    val TailIdents = P( "wd".reverse | "cwd".reverse | "root".reverse | "home".reverse )
    val SegmentIdent = P( "up".reverse )
    val Tail = P( ("/" ~ TailIdents.!.map(_.reverse)).? )
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
