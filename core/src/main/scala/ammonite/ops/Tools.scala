package ammonite.ops

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
    new ammonite.pprint.PPrint(pp.a, pp.cfg.copy(literalColor=null, prefixColor=null))
  }
  implicit object Str extends Grepper[String] {
    def apply[V: ammonite.pprint.PPrint](t: String, s: V) = {
      Regex.apply(scala.util.matching.Regex.quote(t).r, s)
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

    // String: |        S   E       S    E      |
    // Cuts:   0       1 2 3 4     5 6  7 8     9
    while(!remaining.isEmpty){
      val (firstStart, firstEnd) = remaining.head
      val (included, excluded) = remaining.partition{
        case (_, end) => end - firstStart < cfg.maxWidth
      }
      remaining = excluded
      val lastEnd = included.last._2
      val middle = (lastEnd + firstStart) / 2
      val showStart = middle - cfg.maxWidth / 2
      val allIndices = included.flatMap{case (a, b) => Seq(a, b)}
      val pairs = (showStart +: allIndices) zip (allIndices :+ (showStart + cfg.maxWidth))

      val snippets = for(((a, b), i) <- pairs.zipWithIndex) yield {
        gr.txt.slice(a, b) + color(i)
      }
      spans.append(color(1) + snippets.mkString + Console.RESET)
    }

    Iterator(spans.mkString("\n"), "\n")
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