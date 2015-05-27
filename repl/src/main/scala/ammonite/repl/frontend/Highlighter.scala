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
    //      case Literals.Comment => "<B|"
    case ExprLiteral => Console.GREEN
    case Type => Console.GREEN
    case Stringy(BackTicked(body)) if alphaKeywords.contains(body) => Console.YELLOW
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

        for(color <- ruleColors.lift(rule.asInstanceOf[Rule[_]]) if !done){
          val closeColor = indices.last._2
          indices += ((idx, color, true))
          println("Attempt" + indices)
          val resIndex = res() match {
//            case x if x.index == idx =>
//              // Didn't parse anything, ignore
//              println("Ignore " + indices)
//              indices.remove(indices.length - 1)
            case s: Result.Success[_] if s.index > idx =>
              indices += ((s.index, closeColor, false))
            case f: Result.Failure
              if f.index == buffer.length
              && (WL ~ End).parse(input, idx).isInstanceOf[Result.Failure] =>
              // EOF, stop all further parsing
              done = true
            case f =>  // hard failure, discard all progress
              while(indices.last._1 >= idx && indices.last._3)
                indices.remove(indices.length - 1)
          }
        }
      })
      indices
    }
    // Make sure there's an index right at the start and right at the end! This
    // resets the colors at the snippet's end so they don't bleed into later output
    val boundedIndices = indices ++ Seq((9999, endColor, false))
    println(boundedIndices)
    val buffer2 =
      boundedIndices
        .sliding(2)
        .flatMap{case Seq((s, c1, _), (e, c2, _)) => c1 ++ buffer.slice(s, e) }
        .toVector

    buffer2
  }

}
