package ammonite.terminal

import java.io.OutputStreamWriter

import acyclic.file
import scala.annotation.tailrec



// Test Unicode:  漢語;𩶘da
object Term{
  def consoleDim(s: String) = {
    import sys.process._
    Seq("bash", "-c", s"tput $s 2> /dev/tty").!!.trim.toInt
  }
  def main(args: Array[String]): Unit = {
    var history = List.empty[String]
    val selection = AdvancedFilters.SelectionFilter()

    val reader = new java.io.InputStreamReader(System.in)
    rec()
    @tailrec def rec(): Unit = {
      TermCore.readLine(
        Console.MAGENTA + "@ " + Console.RESET,
        reader,
        new OutputStreamWriter(System.out),
        selection orElse
        AdvancedFilters.altFilter orElse
        AdvancedFilters.fnFilter orElse
        ReadlineFilters.navFilter orElse
        ReadlineFilters.CutPasteFilter() orElse
//        Example multiline support by intercepting Enter key
        ReadlineFilters.HistoryFilter(() => history) orElse
        BasicFilters.all,
        // Example displayTransform: underline all non-spaces
        displayTransform = (buffer, cursor) => {
          // underline all non-blank lines
          def hl(b: Vector[Char]): Vector[Char] = b.flatMap{
            case ' ' => " "
            case '\n' => "\n"
            case c => Console.UNDERLINED + c + Console.RESET
          }
          // and highlight the selection
          selection.mark match{
            case Some(mark) if mark != cursor =>
              val Seq(min, max) = Seq(mark, cursor).sorted
              val (a, b0) = buffer.splitAt(min)
              val (b, c) = b0.splitAt(max - min)
              val displayOffset = if (cursor < mark) 0 else -1
              (
                hl(a) ++ Console.REVERSED ++ b ++ Console.RESET ++ hl(c),
                cursor + displayOffset
              )
            case _ => (hl(buffer), cursor)
          }

        }
      ) match {
        case None => println("Bye!")
        case Some(s) =>
          history = s :: history
          println(s)
          rec()
      }
    }
  }
}

