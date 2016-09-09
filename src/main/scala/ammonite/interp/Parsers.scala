package ammonite.runtime

object Parsers {

  import fastparse.noApi._

  import scalaparse.Scala._
  import WhitespaceApi._

  val Splitter = {
    P(StatementBlock(Fail) ~ WL ~ End)
  }

  // For some reason Scala doesn't import this by default
  private val `_` = scalaparse.Scala.`_`

  val ImportSplitter: P[Seq[ammonite.util.ImportTree]] = {
    val IdParser = P((Id | `_`).!).map(
      s => if (s(0) == '`') s.drop(1).dropRight(1) else s
    )
    val Selector = P(IdParser ~ (`=>` ~/ IdParser).?)
    val Selectors = P("{" ~/ Selector.rep(sep = ",".~/) ~ "}")
    val BulkImport = P(`_`).map(
      _ => Seq("_" -> None)
    )
    val Prefix = P(IdParser.rep(1, sep = "."))
    val Suffix = P("." ~/ (BulkImport | Selectors))
    val ImportExpr: P[ammonite.util.ImportTree] = {
      // Manually use `WL0` parser here, instead of relying on WhitespaceApi, as
      // we do not want the whitespace to be consumed even if the WL0 parser parses
      // to the end of the input (which is the default behavior for WhitespaceApi)
      P(Index ~~ Prefix ~~ (WL0 ~~ Suffix).? ~~ Index).map {
        case (start, idSeq, selectors, end) =>
          ammonite.util.ImportTree(idSeq, selectors, start, end)
      }
    }
    P(`import` ~/ ImportExpr.rep(1, sep = ",".~/))
  }

  // private val PatVarSplitter = {
  //   val Prefixes = P(Prelude ~ (`var` | `val`))
  //   val Lhs = P(Prefixes ~/ BindPattern.rep(1, "," ~/ Pass) ~ (`:` ~/ Type).?)
  //   P(Lhs.! ~ (`=` ~/ WL ~ StatCtx.Expr.!) ~ End)
  // }

  private val Prelude = P((Annot ~ OneNLMax).rep ~ (Mod ~/ Pass).rep)

  private val Statement = P(scalaparse.Scala.TopPkgSeq | scalaparse.Scala.Import | Prelude ~ BlockDef | StatCtx.Expr)

  private def StatementBlock(blockSep: P0) = P(Semis.? ~ (!blockSep ~ Statement ~~ WS ~~ (Semis | End)).!.repX)

  /**
    * Attempts to break a code blob into multiple statements. Returns `None` if
    * it thinks the code blob is "incomplete" and requires more input
    */
  def split(code: String): Option[fastparse.core.Parsed[Seq[String]]] =
    Splitter.parse(code) match {
      case Parsed.Failure(_, index, extra) if code.drop(index).trim() == "" =>
        None
      case x => Some(x)
    }

  private val Separator = P(WL ~ "@" ~~ CharIn(" " + System.lineSeparator).rep(1))

  private val CompilationUnit = P(WL.! ~ StatementBlock(Separator) ~ WL)

  private val ScriptSplitter = P(CompilationUnit.repX(1, Separator) ~ End)

  def splitScript(code: String) = ScriptSplitter.parse(code)

  private val BlockUnwrapper = P("{" ~ Block.! ~ "}" ~ End)

  def unwrapBlock(code: String) = {
    BlockUnwrapper.parse(code) match {
      case Parsed.Success(contents, _) => Some(contents)
      case _ => None
    }
  }

}
