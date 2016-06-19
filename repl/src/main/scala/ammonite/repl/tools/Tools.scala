/**
 * User-facing tools. Rather specific, and although they could
 * be built upon as part of a larger program, they're
 */
package ammonite.repl.tools
import acyclic.file
import java.io.{BufferedReader, InputStreamReader}

import ammonite.ops._
import ammonite.terminal.Strings

import scala.collection.{GenTraversableOnce, mutable}
import scala.util.matching.Regex



trait Grepper[T]{
  def apply[V: pprint.PPrint](t: T, s: V)(implicit c: pprint.Config): Option[GrepResult]
}
object Grepper{
  implicit object Str extends Grepper[String] {
    def apply[V: pprint.PPrint](t: String, s: V)(implicit c: pprint.Config) = {
      Regex.apply(java.util.regex.Pattern.quote(t).r, s)
    }
  }

  implicit object Regex extends Grepper[Regex] {
    def apply[V: pprint.PPrint](t: Regex, s: V)(implicit c: pprint.Config) = {
      val txt = pprint.tokenize(s).mkString
      val fansiTxt = fansi.Str(txt, errorMode = fansi.ErrorMode.Sanitize)
      val items = t.findAllIn(fansiTxt.plainText).matchData.toSeq
      if (items.isEmpty) None
      else Some(new GrepResult(items.map(m => (m.start, m.end)), fansiTxt))
    }
  }
}

case class GrepResult(spans: Seq[(Int, Int)], text: fansi.Str)

object GrepResult{
  case class Color(highlight: fansi.Attrs, dotDotDotColor: fansi.Attrs)
  object Color{
    implicit val defaultColor = Color(
      fansi.Back.Yellow ++ fansi.Color.Blue,
      fansi.Attrs.Empty
    )
  }

  implicit def grepResultRepr(implicit highlightColor: Color) =
    pprint.PPrinter[GrepResult]{ (grepResult, cfg) =>
      val outputSnippets = mutable.Buffer.empty[fansi.Str]
      val rangeBuffer = mutable.Buffer.empty[(Int, Int)]
      var remainingSpans = grepResult.spans.toList

      var lastEnd = 0 // Where the last generated snippet ended, to avoid overlap

      // 6 to leave space for ... at start and end
      val width = cfg.width - cfg.depth * 2 - 6

      /**
        * Consume all the matches that have been aggregated in `rangeBuffer` and
        * generate a single result snippet to show the user. Multiple ranges
        * can turn up in the same snippet if they are close enough, and we do
        * some math to make sure snippets...
        *
        * - Do not overlap
        * - Are at most `width` wide
        * - Have `...` if they're truncated on the left or right
        * - Are roughly centered on the ranges they contain, as far as possible
        *   given the above.
        */
      def generateSnippet() = {
        val start = rangeBuffer.head._1
        val end = rangeBuffer.last._2

        val middle = (end + start) / 2

        // Range centered on the midpoint of the first/last indices in rangeBuffer
        val naiveStart = middle - width / 2
        val naiveEnd = middle + width / 2

        val boundaryOffset =
          if (naiveStart < lastEnd) lastEnd - naiveStart
          else if (naiveEnd > grepResult.text.length) grepResult.text.length - naiveEnd
          else 0

        def cap(min: Int, n: Int, max: Int) =
          if (n > max) max
          else if (n < min) min
          else n

        // Range shifted to avoid over-running the start or end of the text
        val shiftedStart = naiveStart + boundaryOffset
        val shiftedEnd = naiveEnd + boundaryOffset

        // Range clamped to 0, in case shifting it caused it to run off the end
        // of the string
        val wideStart = cap(0, shiftedStart, grepResult.text.length)
        val wideEnd = cap(0, shiftedEnd, grepResult.text.length)

        val colorRanges =
          for((rangeStart, rangeEnd) <- rangeBuffer)
          yield (highlightColor.highlight, rangeStart - wideStart, rangeEnd - wideStart)

        val colored = grepResult.text.substring(wideStart, wideEnd).overlayAll(colorRanges)

        val dotDotDot = highlightColor.dotDotDotColor("...")
        val prefix = if (shiftedStart > 0) dotDotDot else fansi.Str("")
        val suffix = if (shiftedEnd < grepResult.text.length) dotDotDot else fansi.Str("")

        outputSnippets.append(prefix ++ colored ++ suffix)
        lastEnd = wideEnd
        rangeBuffer.clear()
      }

      // Keep chomping away at the remaining spans until the next span is beyond
      // the acceptable width, and when that happens consume all the stored spans
      // to generate a snippet. Generate one more snippet at the end too to use up
      // any un-consumed spans
      while(remainingSpans.nonEmpty){
        val (start, end) = remainingSpans.head
        remainingSpans = remainingSpans.tail
        if (rangeBuffer.nonEmpty && end - rangeBuffer(0)._1 >= width) {
          generateSnippet()
        }
        rangeBuffer.append((start, end))
      }
      generateSnippet()

      Iterator(outputSnippets.mkString("\n"))
    }
}


/**
 * Lets you filter a list by searching for a matching string or
 * regex within the pretty-printed contents.
 */
object grep {
  def apply[T: Grepper, V: pprint.PPrint]
           (pat: T, str: V)
           (implicit c: pprint.Config)
           : Option[GrepResult] = {
    implicitly[Grepper[T]].apply(pat, str)
  }

  /**
   * Magic implicits used to turn the [T: PPrint](t: T) => Option[T]
   * into a real T => Option[T] by materializing PPrint[T] for various values of T
   */
  object !{
    implicit def FunkyFunc1[T: pprint.PPrint]
                           (f: ![_])
                           (implicit c: pprint.Config)
                           : T => GenTraversableOnce[GrepResult] = {
      f.apply[T]
    }

    implicit def FunkyFunc2[T: pprint.PPrint]
                           (f: ![_])
                           (implicit c: pprint.Config)
                           : T => Boolean = {
      x => f.apply[T](x).isDefined
    }
  }

  case class ![T: Grepper](pat: T){
    def apply[V: pprint.PPrint](str: V)(implicit c: pprint.Config) = grep.this.apply(pat, str)
  }
}

case class tail(interval: Int, prefix: Int) extends Function[Path, Iterator[String]]{
  def apply(arg: Path): Iterator[String] = {
    val is = java.nio.file.Files.newInputStream(arg.toNIO)
    val br = new BufferedReader(new InputStreamReader(is))
    Iterator.continually{
      val line = br.readLine()
      if (line == null) Thread.sleep(interval)
      Option(line)
    }.flatten

  }
}

/**
 * Follows a file as it is written, handy e.g. for log files.
 * Returns an iterator which you can then print to standard out
 * or use for other things.
 */
object tail extends tail(100, 50)
/**
  * Records how long the given computation takes to run, returning the duration
  * in addition to the return value of that computation
  */
object time {

  def apply[T](t: => T) = {
    val start = System.nanoTime()
    val res = t
    val end = System.nanoTime()

    (res, scala.concurrent.duration.Duration.fromNanos(end - start))
  }
}

object browse{
  // R -> show ansi-colors as colors, M -> show current-browse-% bar
  val lessViewer = Seq("less", "-RM")
  def apply[T: pprint.PPrint](t: T,
                              viewer: Strings = lessViewer,
                              width: Integer = null,
                              height: Integer = null,
                              indent: Integer = null,
                              colors: pprint.Colors = null)
                             (implicit c: pprint.Config,
                              wd: Path = ammonite.ops.ImplicitWd.implicitCwd) = {
    %(
      viewer.values,
      tmp(
        pprint.tokenize(
          t,
          width = width,
          height = -1,
          indent = indent,
          colors = colors
        )
      )
    )
  }
}