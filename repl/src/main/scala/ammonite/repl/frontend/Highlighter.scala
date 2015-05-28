package ammonite.repl.frontend
import acyclic.file
import fastparse.core.Result
import fastparse.parsers.Combinators.Rule
import fastparse.parsers.Terminals.End

import scala.util.matching.Regex
import scalaparse.Scala._
import scalaparse.syntax.Identifiers._
object Highlighter {
  val BackTicked = "`([^`]+)`".r
  object Stringy{ def unapply(s: Any): Option[String] = Some(s.toString)}

  def defaultHighlight(buffer: Vector[Char]) = Highlighter.highlight(
  buffer,
  {
    case Literals.Expr.Interp | Literals.Pat.Interp => Console.RESET
    case Literals.Comment => Console.BLUE
    case ExprLiteral => Console.GREEN
    case SimpleType => Console.GREEN
    case Stringy(BackTicked(body))
      if alphaKeywords.contains(body) => Console.YELLOW
  },
  endColor = Console.RESET
  )
  def highlight(buffer: Vector[Char],
                ruleColors: PartialFunction[Rule[_], String],
                endColor: String = Console.RESET) = {
    val indices = {
      var indices = collection.mutable.Buffer((0, endColor, false))
      var done = false
      val input = buffer.mkString
      ammonite.repl.Parsers.Splitter.parse(input, instrumenter = (rule, idx, res) => {
        for{
          color <- ruleColors.lift(rule.asInstanceOf[Rule[_]])
          if !done // If we're done, do nothing
          if idx >= indices.last._1 // If this is a re-parse, ignore it
          if color != indices.last._2 // If it does't change the color, why bother?
        } {
          val closeColor = indices.last._2
          val startIndex = indices.length
          indices += ((idx, color, true))
          val resIndex = res() match {
            case s: Result.Success[_] =>
              indices += ((s.index, closeColor, false))
              if (s.index == buffer.length) done = true
            case f: Result.Failure
              if f.index == buffer.length
              && (WL ~ End).parse(input, idx).isInstanceOf[Result.Failure] =>
              // EOF, stop all further parsing
              done = true
            case _ =>  // hard failure, or parsed nothing. Discard all progress
              indices.remove(startIndex, indices.length - startIndex)
          }
        }
      })
      indices
    }
    // Make sure there's an index right at the start and right at the end! This
    // resets the colors at the snippet's end so they don't bleed into later output
    val boundedIndices = indices ++ Seq((9999, endColor, false))
//    println(boundedIndices)
    val buffer2 =
      boundedIndices
        .sliding(2)
        .flatMap{case Seq((s, c1, _), (e, c2, _)) => c1 ++ buffer.slice(s, e) }
        .toVector

    buffer2
  }

}
