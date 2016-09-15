package ammonite.runtime

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
  val Statement =
    P( scalaparse.Scala.TopPkgSeq | scalaparse.Scala.Import | Prelude ~ BlockDef | StatCtx.Expr )
  def StatementBlock(blockSep: P0) =
    P( Semis.? ~ (!blockSep ~ Statement ~~ WS ~~ (Semis | End)).!.repX)
  val Splitter = P( StatementBlock(Fail) ~ WL ~ End)

  /**
   * Attempts to break a code blob into multiple statements. Returns `None` if
   * it thinks the code blob is "incomplete" and requires more input
   */
  def split(code: String): Option[Parsed[Seq[String]]] = Splitter.parse(code) match{
    case Parsed.Failure(_, index, extra) if code.drop(index).trim() == "" => None
    case x => Some(x)
  }

  val Separator = P( WL ~ "@" ~~ CharIn(" " + System.lineSeparator).rep(1) )
  val CompilationUnit = P( WL.! ~ StatementBlock(Separator) ~ WL )
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
