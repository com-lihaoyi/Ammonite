package ammonite.shell

import java.io.OutputStreamWriter

import ammonite.terminal._
import Filter._
import ammonite.repl.FrontEndUtils
import ammonite.util.Colors
import ammonite.compiler.Parsers
import ammonite.terminal._
import ammonite.terminal.LazyList.~:
/**
 * Logic to find path "literals" so we can attempt to autocomplete them based
 * on what's actually on the filesystem.
 */
object PathComplete {
  /**
   * @param base An identifier representing the absolute path which this
   *             path literal is based on. If `None`, then we've only
   *             found a relative path
   * @param body The various path segments, as `String`s. `None` means it
   *             is an `up` instead of a literal path segment
   * @param frag The last, potentially incomplete section of the path
   *             just before the cursor
   * @param offset
   */
  case class PathLiteralInfo(base: Option[String],
                             body: Seq[Option[String]],
                             frag: Option[String],
                             offset: Int)
  /**
   * Searches the current snippet for path-like forms ending at the
   * current cursor. Doesn't actually go to the filesystem to see what
   * exists, just returns a [[PathLiteralInfo]] representing the
   * structure of the path-like-syntax it found
   *
   * The overall approach is somewhat convoluted, but roughly:
   *
   * - Use `highlightIndices` to search for all syntactic instances of
   *   strings, identifiers or symbols within the text, since those are
   *   the forms that can comprise path literals
   * - Convert the raw index-tuples into a sequence of objects representing
   *   each found form
   * - Search backwards from the end of the that list. Take up any initial
   *   (possibly half-complete) Symbol/String literal to become the `frag`,
   *   gobble up any `/`s followed by `ups` or Symbol/String literals to
   *   form the path, and stop if you reach a absolute path "literal"
   *   `wd`/`pwd`/`home`/`root` or if you can't parse anything anymore.
   *
   * @param snippet The entire code snippet in which we are requesting
   *                autocomplete
   * @param cursor The position of the cursor when the user hits Tab
   * @return `None` if no autocomplete is possible, otherwise a [[PathLiteralInfo]]
   */
  def findPathLiteral(snippet: String, cursor: Int): Option[PathLiteralInfo] = {
    val indices = Parsers.highlightIndices(
      snippet.toVector,
      {
        case "Id" => Interval.Id
        case "String" => Interval.String
        case "Symbol" => Interval.Symbol
      },
      Interval.End
    )

    val spans =
      indices
        .drop(1)
        // Weird hack to get around other weird hack in Highlighter, where it
        // uses 999999 to represent incomplete input
        .map{case (a, b) if a > 9999 => (cursor, b) case x => x}
        .grouped(2)
        .collect{
          case Seq((s, Interval.Id), (e, Interval.End)) => Span.Id(s, e)
          case Seq((s, Interval.String), (e, Interval.End)) => Span.String(s, e)
          case Seq((s, Interval.Symbol), (e, Interval.End)) => Span.Symbol(s, e)
        }
        .toVector
        .reverse

    spans.headOption match{
      case None => None
      case Some(head) =>
        // None means we're not in a path autocomplete, Some(None) means
        // we are but there is no incomplete segment before the cursor
        val (frag: Option[Option[String]], prev0: Int) =
          head match{
            case span: Span.Id =>
              if (snippet.substring(span.start, span.end) == "/") (Some(None), cursor)
              else (None, 0)
            case span: Span.String if span.end == cursor =>
              (Some(Some(snippet.slice(span.start, span.end))), span.start)
            case span: Span.Symbol if span.end == cursor =>
              (Some(Some(snippet.slice(span.start, span.end))), span.start)
            case _ => (None, 0)
          }

        def rec(prev: Int,
                spans: List[Span],
                segments: List[Option[String]]): (Seq[Option[String]], Option[String]) = {

          spans match{
            case Span.Id(start, end) :: next :: rest
              if snippet.slice(start, end) == "/"
                && snippet.slice(end, prev).trim() == ""
                && snippet.slice(next.end, start).trim() == "" =>

              (next, snippet.slice(next.start, next.end)) match{
                case (_: Span.Id, "up") => rec(next.start, rest, None :: segments)
                case (_: Span.Id, x) if rootMap.keySet.flatten.contains(x) => (segments, Some(x))
                case (_: Span.String, v) =>
                  val mangled = v.drop(1).dropRight(1).replace("\\\"", "\"").replace("\\\\", "\\")
                  rec(next.start, rest, Some(mangled) :: segments)
                case (_: Span.Symbol, v) => rec(next.start, rest, Some(v.drop(1)) :: segments)
                case _ => (segments, None)
              }
            case _ => (segments, None)
          }
        }
        for {
          frag <- frag
          (body, base) = rec(
            prev0,
            if (frag.isDefined) spans.toList.drop(1) else spans.toList,
            Nil
          )
          if !(body ++ frag ++ base).isEmpty
        } yield PathLiteralInfo(base, body, frag, cursor - prev0)
    }
  }

  import ammonite.ops._

  /**
   * Small hard-coded list of how to convert the names of various
   * path-literal-roots into actual Paths. Some depend on the REPL's
   * `wd`, others don't
   */
  val rootMap = Map[Option[String], Path => Path](
    None -> (wd => wd),
    Some("wd") -> (wd => wd),
    Some("pwd") -> (wd => pwd),
    Some("root") -> (wd => root),
    Some("home") -> (wd => home)
  )

  def colorPath(path: Path) = {
    stat(path).fileType match{
      case ammonite.ops.FileType.Dir => fansi.Color.Cyan
      case ammonite.ops.FileType.File => fansi.Color.Green
      case ammonite.ops.FileType.SymLink => fansi.Color.Yellow
      case ammonite.ops.FileType.Other => fansi.Color.Red
    }
  }
  def pathCompleteFilter(wd: => Path,
                         colors: => Colors): Filter = partial{
    case TermInfo(TermState(9 ~: rest, b, c, _), width)
      if PathComplete.findPathLiteral(b.mkString, c).isDefined =>

      val Some(PathComplete.PathLiteralInfo(base, seq, frag, cursorOffset)) =
        PathComplete.findPathLiteral(b.mkString, c)

      val path = rootMap(base)(wd) / seq.map { case None => os.up; case Some(s) => s: RelPath }

      if (!exists(path)) TermState(rest, b, c)
      else {
        val fragPrefix = frag.getOrElse("")

        def wrap(s: String) = "\"" + pprint.Util.literalize(s) + "\""
        val options = (
          ls ! path | (x => (x, wrap(x.last)))
                    |? (_._2.startsWith(fragPrefix))
          )
        val (completions, details) = options.partition(_._2 != fragPrefix)

        val coloredCompletions = for((path, name) <- completions) yield{
          val color = colorPath(path)
          color(name).render
        }




        val details2 = details.map(x => pprint.tokenize(stat(x._1)).mkString)

        val stdout =
          FrontEndUtils.printCompletions(coloredCompletions, details2)
                       .mkString

        if (details.length != 0 || completions.length == 0)
          Printing(TermState(rest, b, c), stdout)
        else {
          val common = FrontEndUtils.findPrefix(completions.map(_._2), 0)
          val newBuffer = b.take(c - cursorOffset) ++ common ++ b.drop(c)
          Printing(TermState(rest, newBuffer, c - cursorOffset + common.length + 1), stdout)
        }
      }
  }

  /**
   * Enum used to tag the indices being returned by [[Parsers]]
   */
  sealed trait Interval
  object Interval{
    object Id extends Interval
    object String extends Interval
    object Symbol extends Interval
    object End extends Interval
  }

  /**
   * More-convenient data-structures to work with, compared to the raw
   * tuple-output of [[Parsers]]
   */
  sealed trait Span{
    def parseStart: Int;
    def end: Int

    /**
     * Different from [[parseStart]], because [[Span.Symbol]] starts
     * parsing one-character late.
     */
    def start: Int = parseStart
  }
  object Span{
    case class Id(parseStart: Int, end: Int) extends Span
    case class String(parseStart: Int, end: Int) extends Span
    case class Symbol(parseStart: Int, end: Int) extends Span{
      override def start = parseStart - 1
    }
  }
}
