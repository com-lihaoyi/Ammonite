package ammonite.kernel

import fastparse.noApi._
import Parsed.Failure
import scalaparse.Scala._
import WhitespaceApi._

private[kernel] object Parsers {

  val splitter = {
    P(statementBlocl(Fail) ~ WL ~ End)
  }

  // For some reason Scala doesn't import this by default
  private val underscore = scalaparse.Scala.`_`

  private val prelude = P((Annot ~ OneNLMax).rep ~ (Mod ~/ Pass).rep)

  private val statement = P(scalaparse.Scala.TopPkgSeq | scalaparse.Scala.Import | prelude ~ BlockDef | StatCtx.Expr)

  private def statementBlocl(blockSep: P0) = P(Semis.? ~ (!blockSep ~ statement ~~ WS ~~ (Semis | End)).!.repX)

  /**
    * Attempts to break a code blob into multiple statements. Returns `None` if
    * it thinks the code blob is "incomplete" and requires more input
    */
  def split(code: String): Option[fastparse.core.Parsed[Seq[String]]] =
    splitter.parse(code) match {
      case Failure(_, index, extra) if code.drop(index).trim() == "" => None
      case x => Some(x)
    }

}
