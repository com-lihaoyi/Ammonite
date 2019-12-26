package ammonite.interp

import ammonite.util.ImportTree
import ammonite.util.Util.CodeSource

import scala.collection.mutable

object Parsers {

  import fastparse._

  import ScalaWhitespace._
  import scalaparse.Scala._

  // For some reason Scala doesn't import this by default
  def `_`[_: P] = scalaparse.Scala.`_`


  def ImportSplitter[_: P]: P[Seq[ammonite.util.ImportTree]] = {
    def IdParser = P( (Id | `_` ).! ).map(
      s => if (s(0) == '`') s.drop(1).dropRight(1) else s
    )
    def Selector = P( IdParser ~ (`=>` ~/ IdParser).? )
    def Selectors = P( "{" ~/ Selector.rep(sep = ","./) ~ "}" )
    def BulkImport = P( `_`).map(
      _ => Seq("_" -> None)
    )
    def Prefix = P( IdParser.rep(1, sep = ".") )
    def Suffix = P( "." ~/ (BulkImport | Selectors) )
    def ImportExpr: P[ammonite.util.ImportTree] = {
      // Manually use `WL0` parser here, instead of relying on WhitespaceApi, as
      // we do not want the whitespace to be consumed even if the WL0 parser parses
      // to the end of the input (which is the default behavior for WhitespaceApi)
      P( Index ~~ Prefix ~~ (WL0 ~~ Suffix).? ~~ Index).map{
        case (start, idSeq, selectors, end) =>
          ammonite.util.ImportTree(idSeq, selectors, start, end)
      }
    }
    P( `import` ~/ ImportExpr.rep(1, sep = ","./) )
  }

  def PatVarSplitter[_: P] = {
    def Prefixes = P(Prelude ~ (`var` | `val`))
    def Lhs = P( Prefixes ~/ BindPattern.rep(1, "," ~/ Pass) ~ (`:` ~/ Type).? )
    P( Lhs.! ~ (`=` ~/ WL ~ StatCtx.Expr.!) ~ End )
  }
  def patVarSplit(code: String) = {
    val Parsed.Success((lhs, rhs), _) = parse(code, PatVarSplitter(_))
    (lhs, rhs)
  }

  def Prelude[_: P] = P( (Annot ~ OneNLMax).rep ~ (Mod ~/ Pass).rep )

  def TmplStat[_: P] = P( Import | Prelude ~ BlockDef | StatCtx.Expr )


  // Do this funny ~~WS thing to make sure we capture the whitespace
  // together with each statement; otherwise, by default, it gets discarded.
  //
  // After each statement, there must either be `Semis`, a "}" marking the
  // end of the block, or the `End` of the input
  def StatementBlock[_: P](blockSep: => P0) =
    P( Semis.? ~ (!blockSep ~ TmplStat ~~ WS ~~ (Semis | &("}") | End)).!.repX)

  def Splitter0[_: P] = P( StatementBlock(Fail) )
  def Splitter[_: P] = P( ("{" ~ Splitter0 ~ "}" | Splitter0) ~ End )

  def ObjParser[_: P] = P( ObjDef )

  /**
   * Attempts to break a code blob into multiple statements. Returns `None` if
   * it thinks the code blob is "incomplete" and requires more input
   */
  def split(code: String): Option[Parsed[Seq[String]]] = {
    // We use `instrument` to detect when the parser has reached the end of the
    // input, any time during the parse. If it has done so, and failed, we
    // consider the input incomplete.
    var furthest = 0
    val instrument = new fastparse.internal.Instrument {
      def beforeParse(parser: String, index: Int): Unit = ()
      def afterParse(parser: String, index: Int, success: Boolean): Unit = {
        if (index > furthest) furthest = index
      }
    }

    parse(code, Splitter(_), instrument = instrument) match{
      case Parsed.Failure(_, index, extra) if furthest == code.length => None
      case x => Some(x)
    }
  }

  def isObjDef(code: String): Boolean = {
    parse(code, ObjParser(_))
      .fold((_, _, _) => false, (_, _) => true)
  }

  def Separator[_: P] = P( WL ~ "@" ~~ CharIn(" \n\r").rep(1) )
  def CompilationUnit[_: P] = P( WL.! ~ StatementBlock(Separator) ~ WL )
  def ScriptSplitter[_: P] = P( CompilationUnit.repX(1, Separator) ~ End)
  def splitScript(code: String) = parse(code, ScriptSplitter(_))

  def stringWrap(s: String) = "\"" + pprint.Util.literalize(s) + "\""
  def stringSymWrap(s: String) = {
    def idToEnd[_: P] = P( scalaparse.syntax.Identifiers.Id ~ End )
    if (s == "") "'"
    else parse(s, idToEnd(_))  match{
      case Parsed.Success(v, _) =>  "'" + s
      case f: Parsed.Failure => stringWrap(s)
    }
  }
  def parseImportHooks(source: CodeSource, stmts: Seq[String]) = synchronized{
    val hookedStmts = mutable.Buffer.empty[String]
    val importTrees = mutable.Buffer.empty[ImportTree]
    for(stmt <- stmts) {
      // Call `fastparse.ParserInput.fromString` explicitly, to avoid generating a
      // lambda in the class body and making the we-do-not-load-fastparse-on-cached-scripts
      // test fail
      parse(fastparse.ParserInput.fromString(stmt), Parsers.ImportSplitter(_)) match{
        case f: Parsed.Failure => hookedStmts.append(stmt)
        case Parsed.Success(parsedTrees, _) =>
          var currentStmt = stmt
          for(importTree <- parsedTrees){
            if (importTree.prefix(0)(0) == '$') {
              val length = importTree.end - importTree.start
              currentStmt = currentStmt.patch(
                importTree.start, (importTree.prefix(0) + ".$").padTo(length, ' '), length
              )
              importTrees.append(importTree)
            }
          }
          hookedStmts.append(currentStmt)
      }
    }
    (hookedStmts.toSeq, importTrees.toSeq)
  }
}
