package ammonite.repl.frontend
import acyclic.file
import fastparse.all._
import fastparse.parsers.Combinators.Rule
import scalaparse.Scala._
import scalaparse.syntax.Identifiers._
object Highlighter {

  object BackTicked{
    def unapplySeq(s: Any): Option[List[String]] = {
      "`([^`]+)`".r.unapplySeq(s.toString)
    }
  }


  def flattenIndices(boundedIndices: Seq[(Int, fansi.Attrs, Boolean)],
                     buffer: Vector[Char]) = {

    boundedIndices
      .sliding(2)
      .map{case Seq((s, c1, _), (e, c2, _)) =>
        assert(e >= s, s"s: $s e: $e")
        c1(fansi.Str(buffer.slice(s, e)))
      }.reduce(_ ++ _).render.toVector
  }

  def defaultHighlight(buffer: Vector[Char],
                       comment: fansi.Attrs,
                       `type`: fansi.Attrs,
                       literal: fansi.Attrs,
                       keyword: fansi.Attrs,
                       reset: fansi.Attrs) = {
    val boundedIndices = defaultHighlightIndices(buffer, comment, `type`, literal, keyword, reset)
    flattenIndices(boundedIndices, buffer)
  }
  def defaultHighlightIndices(buffer: Vector[Char],
                              comment: fansi.Attrs,
                              `type`: fansi.Attrs,
                              literal: fansi.Attrs,
                              keyword: fansi.Attrs,
                              reset: fansi.Attrs) = Highlighter.highlightIndices(
    ammonite.repl.Parsers.Splitter,
    buffer,
    {
      case Literals.Expr.Interp | Literals.Pat.Interp => reset
      case Literals.Comment => comment
      case ExprLiteral => literal
      case TypeId => `type`
      case BackTicked(body)
        if alphaKeywords.contains(body) => keyword
    },
    reset
  )
  def highlightIndices[T](parser: fastparse.core.Parser[_],
                          buffer: Vector[Char],
                          ruleColors: PartialFunction[Rule[_], T],
                          endColor: T): Seq[(Int, T, Boolean)] = {
    val indices = {
      var indices = collection.mutable.Buffer((0, endColor, false))
      var done = false
      val input = buffer.mkString
      parser.parse(input, instrument = (rule, idx, res) => {
        for(color <- ruleColors.lift(rule.asInstanceOf[Rule[_]])){
          val closeColor = indices.last._2
          val startIndex = indices.length
          indices += ((idx, color, true))

          res() match {
            case s: Parsed.Success[_] =>
              val prev = indices(startIndex - 1)._1

              if (idx < prev && s.index <= prev){
                indices.remove(startIndex, indices.length - startIndex)

              }
              while (idx < indices.last._1 && s.index <= indices.last._1) {
                indices.remove(indices.length - 1)
              }
              indices += ((s.index, closeColor, false))
              if (s.index == buffer.length) done = true
            case f: Parsed.Failure
              if f.index == buffer.length
              && (WL ~ End).parse(input, idx).isInstanceOf[Parsed.Failure] =>
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
    indices ++ Seq((999999999, endColor, false))
  }
  def highlight(parser: Parser[_],
                buffer: Vector[Char],
                ruleColors: PartialFunction[Rule[_], fansi.Attrs],
                endColor: fansi.Attrs) = {
    val boundedIndices = highlightIndices(parser, buffer, ruleColors, endColor)
    flattenIndices(boundedIndices, buffer)
  }

}
