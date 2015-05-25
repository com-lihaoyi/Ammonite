/**
 * User-facing tools. Rather specific, and although they could
 * be built upon as part of a larger program, they're
 */
package ammonite.ops

import java.io.{InputStreamReader, BufferedReader}

import ammonite.pprint
import ammonite.pprint.PPrint

import scala.collection.{GenTraversableOnce, mutable}
import scala.util.matching.Regex

trait Grepper[T]{
  def apply[V: ammonite.pprint.PPrint](t: T, s: V): Option[GrepResult]
}
object Grepper{
  def BlackWhite[V: PPrint] = {
    val pp = implicitly[PPrint[V]]
    new ammonite.pprint.PPrint(pp.pprinter, pp.cfg.copy(literalColor=null, prefixColor=null))
  }
  implicit object Str extends Grepper[String] {
    def apply[V: ammonite.pprint.PPrint](t: String, s: V) = {
      Regex.apply(java.util.regex.Pattern.quote(t).r, s)
    }
  }

  implicit object Regex extends Grepper[Regex] {
    def apply[V: ammonite.pprint.PPrint](t: Regex, s: V) = {
      val txt = ammonite.pprint.PPrint(s)(BlackWhite).mkString
      val items = t.findAllIn(txt).matchData.toSeq
      if (items.length == 0) None
      else{
        Some(
          new GrepResult(items.map(m => (m.start, m.end)), txt)
        )
      }
    }
  }
}

object GrepResult{
  implicit def grepResultRepr(implicit cfg: pprint.Config) = pprint.PPrinter[GrepResult]{ (gr, c) =>
    val spans = mutable.Buffer.empty[String]
    var remaining = gr.spans.toList

    def color(i: Int) =
      if(i % 2 == 1) Console.RESET + cfg.literalColor
      else Console.YELLOW_B + Console.BLUE

    val width = cfg.maxWidth - cfg.depth * 2
    // String: |        S   E       S    E      |
    // Cuts:   0       1 2 3 4     5 6  7 8     9
    while(!remaining.isEmpty){
      val (firstStart, firstEnd) = remaining.head
      val (included, excluded) = remaining.partition{
        case (_, end) => end - firstStart < width
      }
      remaining = excluded
      val lastEnd = included.last._2
      val middle = (lastEnd + firstStart) / 2
      val showStart = middle - cfg.maxWidth / 2
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
  def apply[T: Grepper, V: pprint.PPrint](pat: T, str: V): Option[GrepResult] = {
    implicitly[Grepper[T]].apply(pat, str)
  }

  /**
   * Magic implicits used to turn the [T: PPrint](t: T) => Option[T]
   * into a real T => Option[T] by materializing PPrint[T] for various values of T
   */
  object !{
    implicit def FunkyFunc1[T: pprint.PPrint](f: ![_]): T => GenTraversableOnce[GrepResult] = f.apply[T]
    implicit def FunkyFunc2[T: pprint.PPrint](f: ![_]): T => Boolean = x => f.apply[T](x).isDefined
  }

  case class ![T: Grepper](pat: T){
    def apply[V: ammonite.pprint.PPrint](str: V) = grep.this.apply(pat, str)
  }
}

/**
 * Used to split a stream down two or three different path, e.g. to
 * print out intermediate values in the middle of a longer pipeline
 */
object tee {
  def apply[T, R1, R2](f1: T => R1, f2: T => R2) = (v: T) => (f1(v), f2(v))
  def apply[T, R1, R2, R3](f1: T => R1, f2: T => R2, f3: T => R3) = (v: T) => (f1(v), f2(v), f3(v))
}

case class tail(interval: Int, prefix: Int) extends Op1[Path, Iterator[String]]{
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