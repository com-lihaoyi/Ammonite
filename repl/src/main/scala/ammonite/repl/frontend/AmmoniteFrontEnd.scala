package ammonite.repl.frontend

import java.io.{OutputStreamWriter, OutputStream, InputStream}

import ammonite.repl.{Parsers, Res, Timer, Colors}
import ammonite.terminal.LazyList.~:
import ammonite.terminal._
import fastparse.core.Result

import scala.annotation.tailrec

object AmmoniteFrontEnd extends FrontEnd{

  def width = ammonite.terminal.TTY.consoleDim("cols")
  def height = ammonite.terminal.TTY.consoleDim("lines")
  def tabulate(snippets: Seq[String], width: Int) = {
    val gap = 2
    val maxLength = snippets.maxBy(_.replaceAll("\u001B\\[[;\\d]*m", "").length).length + gap
    val columns = math.max(1, width / maxLength)

    snippets.grouped(columns).flatMap{
      case first :+ last => first.map(_.padTo(width / columns, ' ')) :+ last :+ "\n"
    }
  }
  @tailrec def findPrefix(a: String, b: String, index: Int): String = {
    if (index >= a.length || index >= b.length || a(index) != b(index)) b.take(index)
    else findPrefix(a, b, index + 1)
  }

  def action(input: InputStream,
             reader: java.io.Reader,
             output: OutputStream,
             prompt: String,
             colors: Colors,
             compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
             history: Seq[String],
             addHistory: String => Unit) = {
    Timer("AmmoniteFrontEnd.action start")
    val res = readLine(reader, output, prompt, colors, compilerComplete, history) match{
      case None => Res.Exit(())
      case Some(code) =>
        addHistory(code)
        Parsers.Splitter.parse(code) match{
          case Result.Success(value, idx) => Res.Success((code, value))
          case f: Result.Failure => Res.Failure(
            fastparse.core.SyntaxError.msg(f.input, f.traced.expected, f.index)
          )
        }
    }
    Timer("AmmoniteFrontEnd.action end")
    res
  }

  def doSomething(completions: Seq[String],
                  details: Seq[String],
                  writer: OutputStreamWriter,
                  rest: LazyList[Int],
                  b: Vector[Char],
                  c: Int) = {
    // If we find nothing, we find nothing
    if (completions.length == 0 && details.length == 0) None
    else {
      writer.write("\n")
      details.foreach(writer.write)
      writer.write("\n")
    }

    writer.flush()
    // If we find no completions, we've already printed the details to abort
    if (completions.length == 0) None
    else {

      tabulate(completions, AmmoniteFrontEnd.this.width).foreach(writer.write)
      writer.flush()
      // If the current prefix already matches a detailed result, we've
      // already printed the messages but no need to modify buffer
      if (details.length != 0) None
      else Some()
    }
  }

  def readLine(reader: java.io.Reader,
               output: OutputStream,
               prompt: String,
               colors: Colors,
               compilerComplete: (Int, String) => (Int, Seq[String], Seq[String]),
               history: Seq[String]) = {
    Timer("AmmoniteFrontEnd.readLine start")
    val writer = new OutputStreamWriter(output)

    def revPath(b: Vector[Char], c: Int) = {
      Parsers.PathComplete.RevPath.parse(b.take(c).reverse.mkString, 0)
    }
    import ammonite.ops._
    val rootMap = Map(
      None -> cwd,
      Some("wd") -> cwd,
      Some("root") -> root,
      Some("home") -> home
    )
    def okRevPath(v: Result[(Option[String], Seq[Option[String]], Option[String], Int)]) = v match{
      case _: Result.Failure => false
      case Result.Success((base, seq, frag, i), _) =>
        rootMap.keySet.contains(base) && (seq ++ base ++ frag).length > 0
    }

    val autocompleteFilter: TermCore.Filter = {
      case TermInfo(TermState(9 ~: rest, b, c), width) if okRevPath(revPath(b, c)) =>

        val Result.Success((base, seq, frag, cursorOffset), _) = revPath(b, c)

        val path = rootMap(base)/seq.map{case None => up; case Some(s) => s: RelPath}

        if (exists(path)) {
          val options = ls ! path |? (_.last.startsWith(frag.getOrElse("")))
          val (completions, details) = options.partition(_.last != frag.getOrElse(""))

          val completions2 = completions.map(_.last)
            .map(Parsers.PathComplete.stringSymWrap)
            .map(colors.literal() + _ + colors.reset())

          import pprint.Config.Colors.PPrintConfig
          val details2 = details.map(x => pprint.tokenize(stat(x)).mkString)
          lazy val common = findPrefix(completions.head.last, completions.last.last, 0)
          doSomething(completions2, details2, writer, rest, b, c) match{
            case None => TermState(rest, b, c)
            case Some(_) =>
              val newBuffer =
                b.take(c - cursorOffset) ++
                Parsers.PathComplete.stringSymWrap(common) ++
                b.drop(c)

              TermState(rest, newBuffer, c - cursorOffset + common.length + 1)
          }
        }else TermState(rest, b, c)


      case TermInfo(TermState(9 ~: rest, b, c), width) =>
        val (newCursor, completions, details) = compilerComplete(c, b.mkString)
        val details2 = for (d <- details) yield {
          Highlighter.defaultHighlight(
            d.toVector,
            colors.comment(),
            colors.`type`(),
            colors.literal(),
            colors.keyword(),
            colors.reset()
          ).mkString
        }
        lazy val common = findPrefix(completions.head, completions.last, 0)
        val completions2 = for(comp <- completions) yield {

          val (left, right) = comp.splitAt(common.length)
          colors.comment() + left + colors.reset() + right
        }
        doSomething(completions2, details2, writer, rest, b, c) match{
          case None => TermState(rest, b, c)
          case Some(_) =>
            val newBuffer = b.take(newCursor) ++ common ++ b.drop(c)
            TermState(rest, newBuffer, newCursor + common.length)
        }

    }

    val multilineFilter: TermCore.Filter = {
      case TermState(13 ~: rest, b, c) => // Enter
        val code = b.mkString
        ammonite.repl.Parsers.split(code) match {
          case None =>
            val (first, last) = b.splitAt(c)
            TermState(rest, (first :+ '\n') ++ last, c + 1)
          case Some(_)  =>
            ammonite.terminal.Result(code)
        }
    }

    val historyFilter = ReadlineFilters.HistoryFilter(() => history.reverse)
    val cutPasteFilter = ReadlineFilters.CutPasteFilter()
    val selectionFilter = GUILikeFilters.SelectionFilter()

    val allFilters =
      selectionFilter orElse
      GUILikeFilters.altFilter orElse
      GUILikeFilters.fnFilter orElse
      ReadlineFilters.navFilter orElse
      autocompleteFilter orElse
      historyFilter.filter orElse
      cutPasteFilter orElse
      multilineFilter orElse
      BasicFilters.all

    Timer("AmmoniteFrontEnd.readLine 1")
    val res = TermCore.readLine(
      prompt,
      reader,
      writer,
      allFilters,
      displayTransform = { (buffer, cursor) =>
        val indices = Highlighter.defaultHighlightIndices(
          buffer,
          colors.comment(),
          colors.`type`(),
          colors.literal(),
          colors.keyword(),
          colors.reset()
        )
        selectionFilter.mark match{
          case Some(mark) if mark != cursor =>
            val Seq(min, max) = Seq(cursor, mark).sorted
            val before = indices.filter(_._1 <= min)
            val after = indices.filter(_._1 > max)
            val lastBeforeAfter =
              indices.filter(_._1 <= max)
                .lastOption
                .map(_._2)
                .getOrElse("")
            val middle = Seq(
              (min, colors.reset() + colors.selected(), true),
              (max, colors.reset() + lastBeforeAfter, true)
            )
            val newIndices = before ++ middle ++ after
            val displayOffset = if (cursor < mark) 0 else -1
            // Add Console.RESET to the end of the buffer to hack around bug
            // in TermCore, to be fixed later when we clean up the crazy
            // TermCore.readLine logic
            (
              Highlighter.flattenIndices(newIndices, buffer) ++ Console.RESET,
              cursor + displayOffset
            )
          case _ => (Highlighter.flattenIndices(indices, buffer) ++ Console.RESET, cursor)
        }
      }
    )
    Timer("TermCore.readLine")
    res
  }
}
