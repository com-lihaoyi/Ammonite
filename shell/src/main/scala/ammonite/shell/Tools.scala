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
      else{
        Some(
          new GrepResult(items.map(m => (m.start, m.end)), txt)
        )
      }
    }
  }
}

object GrepResult{
  implicit def grepResultRepr = pprint.PPrinter[GrepResult]{ (gr, cfg) =>
    val spans = mutable.Buffer.empty[String]
    var remaining = gr.spans.toList

    def color(i: Int) =
      if(i % 2 == 1) Console.RESET + cfg.colors.literalColor
      else Console.YELLOW_B + Console.BLUE

    val width = cfg.width - cfg.depth * 2
    // String: |        S   E       S    E      |
    // Cuts:   0       1 2 3 4     5 6  7 8     9
    while(remaining.nonEmpty){
      val (firstStart, firstEnd) = remaining.head
      val (included, excluded) = remaining.partition{
        case (_, end) => end - firstStart < width
      }
      remaining = excluded
      val lastEnd = included.last._2
      val middle = (lastEnd + firstStart) / 2
      val showStart = middle - cfg.width / 2
      val allIndices = included.flatMap{case (a, b) => Seq(a, b)}
      val pairs = (showStart +: allIndices) zip (allIndices :+ (showStart + width))

      val snippets = for(((a, b), i) <- pairs.zipWithIndex) yield {
        gr.txt.slice(a, b) + color(i)
      }
      spans.append(color(1) + snippets.mkString + Console.RESET)
    }

    Iterator(spans.mkString("\n"))
  }
}

case class GrepResult(spans: Seq[(Int, Int)], txt: String)
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