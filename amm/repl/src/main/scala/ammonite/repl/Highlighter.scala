package ammonite.repl


import ammonite.interp.Parsers

import fastparse._, NoWhitespace._

import scalaparse.Scala._
import scalaparse.syntax.Identifiers._
object Highlighter {

  object BackTicked{
    private[this] val regex = "`([^`]+)`".r
    def unapplySeq(s: String): Option[List[String]] = {
      regex.unapplySeq(s)
    }
  }


  def flattenIndices(boundedIndices: Seq[(Int, fansi.Attrs)],
                     buffer: Vector[Char]) = {

    boundedIndices
      .sliding(2)
      .map{case Seq((s, c1), (e, c2)) =>
        assert(e >= s, s"s: $s e: $e")
        c1(fansi.Str(buffer.slice(s, e), errorMode = fansi.ErrorMode.Sanitize))
      }.reduce(_ ++ _).render.toVector
  }

  def defaultHighlight(buffer: Vector[Char],
                       comment: fansi.Attrs,
                       `type`: fansi.Attrs,
                       literal: fansi.Attrs,
                       keyword: fansi.Attrs,
                       reset: fansi.Attrs) = {
    defaultHighlight0(Parsers.Splitter(_), buffer, comment, `type`, literal, keyword, reset)
  }

  def defaultHighlight0(parser: P[_] => P[Any],
                        buffer: Vector[Char],
                        comment: fansi.Attrs,
                        `type`: fansi.Attrs,
                        literal: fansi.Attrs,
                        keyword: fansi.Attrs,
                        reset: fansi.Attrs) = {
    val boundedIndices =
      defaultHighlightIndices0(parser, buffer, comment, `type`, literal, keyword, reset)

    flattenIndices(boundedIndices, buffer)
  }
  def defaultHighlightIndices(buffer: Vector[Char],
                              comment: fansi.Attrs,
                              `type`: fansi.Attrs,
                              literal: fansi.Attrs,
                              keyword: fansi.Attrs,
                              reset: fansi.Attrs) = Highlighter.defaultHighlightIndices0(
    Parsers.Splitter(_), buffer, comment, `type`, literal, keyword, reset
  )
  def defaultHighlightIndices0(parser: P[_] => P[Any],
                               buffer: Vector[Char],
                               comment: fansi.Attrs,
                               `type`: fansi.Attrs,
                               literal: fansi.Attrs,
                               keyword: fansi.Attrs,
                               reset: fansi.Attrs) = Highlighter.highlightIndices(
    parser,
    buffer,
    {
      case "Interp" => reset
      case "Comment" => comment
      case "ExprLiteral" => literal
      case "TypeId" => `type`
      case BackTicked(body)
        if parse(body, scalaparse.syntax.Identifiers.AlphabetKeywords(_)).isSuccess => keyword
    },
    reset
  )

  def highlightIndices[T](parser: P[_] => P[Any],
                          buffer: Vector[Char],
                          ruleColors: PartialFunction[String, T],
                          endColor: T): Seq[(Int, T)] = {
    val indices = {
      var indices = collection.mutable.Buffer((0, endColor))
      var done = false
      val input = buffer.mkString
      val stack = collection.mutable.ArrayBuffer.empty[(T, Int, Int, Boolean)]
      val res = parse(input, parser, instrument = new fastparse.internal.Instrument {
        def beforeParse(parser: String, index: Int): Unit = {
          for(color <- ruleColors.lift(parser)) {
            val closeColor = indices.last._2
            val startIndex = indices.length
            val newIndex =
              index > indices.lastOption.fold(0)(_._1) ||
              indices.lastOption.map(_._2).contains(endColor)

            if (newIndex) indices += ((index, color))
            stack.append((closeColor, startIndex, index, newIndex))
          }

        }
        def afterParse(parser: String, index: Int, success: Boolean): Unit = {

          for(color <- ruleColors.lift(parser)) {
            val (closeColor, startIndex, idx, newIndex) = stack.remove(stack.length - 1)

            def endCheckParser[_: P] = P(WL ~ End)


            if (newIndex) {
              if (success) {
                val prev = indices(startIndex - 1)._1

                if (idx < prev && index <= prev) {
                  indices.remove(startIndex, indices.length - startIndex)

                }
                while (idx < indices.last._1 && index <= indices.last._1) {
                  indices.remove(indices.length - 1)
                }
                indices += ((index, closeColor))
                if (index == buffer.length) done = true
              } else if (
                  index == buffer.length &&
                  !parse(input, endCheckParser(_), startIndex = startIndex).isSuccess &&
                  index > idx) {
                done = true
              } else {
                indices.remove(startIndex, indices.length - startIndex)
              }
            }

          }

        }
      })

      indices
    }
    // Make sure there's an index right at the start and right at the end! This
    // resets the colors at the snippet's end so they don't bleed into later output
    (indices ++ Seq((999999999, endColor))).toSeq
  }

}
