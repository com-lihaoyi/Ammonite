/**
 * User-facing tools. Rather specific, and although they could
 * be built upon as part of a larger program, they're
 */
package ammonite.shell

import java.io.{InputStreamReader, BufferedReader}

import ammonite.ops._

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
      val items = t.findAllIn(txt).matchData.toSeq
      if (items.isEmpty) None
      else Some(new GrepResult(items.map(m => (m.start, m.end)), txt))
    }
  }
}

object GrepResult{
  case class Color(start: String, end: String)
  object Color{
    implicit val defaultColor = Color(Console.YELLOW_B + Console.BLUE, Console.RESET)
  }
  val ansiRegex = "\u001B\\[[;\\d]*.".r
  implicit def grepResultRepr(implicit highlightColor: Color) =
    pprint.PPrinter[GrepResult]{ (grepResult, cfg) =>
      val spans = mutable.Buffer.empty[String]
      var remaining = grepResult.spans.toList

      def color(i: Int) =
        if(i % 2 == 1) highlightColor.end + cfg.colors.literalColor
        else highlightColor.start

      val width = cfg.width - cfg.depth * 2 - 6 // 6 to leave space for ... at start and end

      def findColoredPoint(index: Int, count: Int, targetOffset: Int): Int = {
        // Sanity checks
        assert(index >= 0, s"index $index must be non-negative")
        assert(count >= 0, s"count $count must be non-negative")
        if (count >= targetOffset) index
        else{
          val subbed = grepResult.text.drop(index)
          ansiRegex.findPrefixMatchOf(subbed) match{
            case Some(matched) => findColoredPoint(index + matched.end, count, targetOffset)
            case None  => findColoredPoint(index+1, count+1, targetOffset)
          }
        }
      }

      // String: |        S   E       S    E      |
      // Cuts:   0       1 2 3 4     5 6  7 8     9
      var previousEnd = 0
      while(remaining.nonEmpty){
        val (firstStart, firstEnd) = remaining.head
        val (included, excluded) = remaining.partition{
          case (_, end) => end - firstStart < width
        }
        remaining = excluded
        val lastEnd = included.last._2
        val middle = {
          val offset = (lastEnd + firstStart) / 2 - firstStart
          findColoredPoint(0, 0, firstStart + offset)
        }

        val blackWhiteMiddle = ansiRegex.replaceAllIn(grepResult.text.take(middle), "").length
        val (showStart, showEnd) = {
          if (blackWhiteMiddle - width / 2 < previousEnd) {
            // If the range is clipping the start of string, take until full width
            (previousEnd, findColoredPoint(previousEnd, 0, width))
          } else if (blackWhiteMiddle + width / 2 >= grepResult.text.length) {
            // If the range is clipping the end of string, start from the full width
            (findColoredPoint(0, 0, grepResult.text.length - width), grepResult.text.length-1)
          } else {
            (
              findColoredPoint(0, 0, blackWhiteMiddle - width / 2),
              findColoredPoint(0, 0, blackWhiteMiddle + width / 2)
            )
          }
        }
        previousEnd = showEnd
        val allIndices = included.flatMap{case (a, b) => Seq(a, b)}

        val pairs = (showStart +: allIndices) zip (allIndices :+ showEnd)

        val snippets = for(((a, b), i) <- pairs.zipWithIndex) yield {
          grepResult.text.slice(a, b) + (if (i < pairs.length-1) color(i) else "")
        }

        val dots = cfg.colors.prefixColor + "..." + cfg.colors.endColor
        val prefixDots = if (showStart > 0) dots else ""
        val suffixDots = if (showEnd < grepResult.text.length - 1) dots else ""

        spans.append(prefixDots + snippets.mkString + suffixDots)
      }

      Iterator(spans.mkString("\n"))
    }
}

case class GrepResult(spans: Seq[(Int, Int)], text: String)
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
    val is = java.nio.file.Files.newInputStream(arg.nio)
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