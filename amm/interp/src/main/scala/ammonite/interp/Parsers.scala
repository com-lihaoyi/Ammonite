package ammonite.interp

object Parsers {

  import fastparse.noApi._

  import scalaparse.Scala._
  import WhitespaceApi._

  // For some reason Scala doesn't import this by default
  val `_` = scalaparse.Scala.`_`


  val ImportSplitter: P[Seq[ammonite.util.ImportTree]] = {
    val IdParser = P( (Id | `_` ).! ).map(
      s => if (s(0) == '`') s.drop(1).dropRight(1) else s
    )
    val Selector = P( IdParser ~ (`=>` ~/ IdParser).? )
    val Selectors = P( "{" ~/ Selector.rep(sep = ",".~/) ~ "}" )
    val BulkImport = P( `_`).map(
      _ => Seq("_" -> None)
    )
    val Prefix = P( IdParser.rep(1, sep = ".") )
    val Suffix = P( "." ~/ (BulkImport | Selectors) )
    val ImportExpr: P[ammonite.util.ImportTree] = {
      // Manually use `WL0` parser here, instead of relying on WhitespaceApi, as
      // we do not want the whitespace to be consumed even if the WL0 parser parses
      // to the end of the input (which is the default behavior for WhitespaceApi)
      P( Index ~~ Prefix ~~ (WL0 ~~ Suffix).? ~~ Index).map{
        case (start, idSeq, selectors, end) =>
          ammonite.util.ImportTree(idSeq, selectors, start, end)
      }
    }
    P( `import` ~/ ImportExpr.rep(1, sep = ",".~/) )
  }

  val PatVarSplitter = {
    val Prefixes = P(Prelude ~ (`var` | `val`))
    val Lhs = P( Prefixes ~/ BindPattern.rep(1, "," ~/ Pass) ~ (`:` ~/ Type).? )
    P( Lhs.! ~ (`=` ~/ WL ~ StatCtx.Expr.!) ~ End )
  }
  def patVarSplit(code: String) = {
    val Parsed.Success((lhs, rhs), _) = PatVarSplitter.parse(code)
    (lhs, rhs)
  }

  val Prelude = P( (Annot ~ OneNLMax).rep ~ (Mod ~/ Pass).rep )

  val TmplStat = P( Import | Prelude ~ BlockDef | StatCtx.Expr )


  // Do this funny ~~WS thing to make sure we capture the whitespace
  // together with each statement; otherwise, by default, it gets discarded.
  //
  // After each statement, there must either be `Semis`, a "}" marking the
  // end of the block, or the `End` of the input
  def StatementBlock(blockSep: P0) =
    P( Semis.? ~ (!blockSep ~ TmplStat ~~ WS ~~ (Semis | &("}") | End)).!.repX)

  val Splitter0 = P( StatementBlock(Fail) )
  val Splitter = P( ("{" ~ Splitter0 ~ "}" | Splitter0) ~ End )

  /**
   * Attempts to break a code blob into multiple statements. Returns `None` if
   * it thinks the code blob is "incomplete" and requires more input
   */
  def split(code: String): Option[Parsed[Seq[String]]] = {
    // We use `instrument` to detect when the parser has reached the end of the
    // input, any time during the parse. If it has done so, and failed, we
    // consider the input incomplete.
    var furthest = 0
    def instrument(p: fastparse.all.Parser[_], index: Int, parse: () => fastparse.all.Parsed[_]) = {
      if (index > furthest) furthest = index
    }

    Splitter.parse(code, instrument = instrument) match{
      case Parsed.Failure(_, index, extra) if furthest == code.length => None
      case x => Some(x)
    }
  }

  val Separator = P( WL ~ "@" ~~ CharIn(" " + System.lineSeparator).rep(1) )
  val CompilationUnit = P( WL.! ~ StatementBlock(Separator) ~ WL )
  val ScriptSplitter = P( CompilationUnit.repX(1, Separator) ~ End)
  def splitScript(code: String) = ScriptSplitter.parse(code)

  def stringWrap(s: String) = "\"" + pprint.Util.literalize(s) + "\""
  def stringSymWrap(s: String) = {
    if (s == "") "'"
    else (scalaparse.syntax.Identifiers.Id ~ End).parse(s, 0)  match{
      case Parsed.Success(v, _) =>  "'" + s
      case f: Parsed.Failure => stringWrap(s)
    }
  }
}
