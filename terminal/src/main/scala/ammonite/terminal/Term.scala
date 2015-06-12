package ammonite.terminal

import acyclic.file
import scala.annotation.tailrec



// Test Unicode:  漢語;𩶘da
object Term{
  def main(args: Array[String]): Unit = {
    var history = List.empty[String]
    val selection = new AdvancedFilters.SelectionFilter
    rec()
    @tailrec def rec(): Unit = {
      TermCore.readLine(
        "@ ",
        System.in,
        System.out,
        selection orElse
        AdvancedFilters.advancedNavFilter orElse
        ReadlineFilters.navFilter orElse
        new ReadlineFilters.cutPasteFilter() orElse
//        Example multiline support by intercepting Enter key
        new ReadlineFilters.HistoryFilter(history) orElse
        BasicFilters.multilineFilter orElse
        BasicFilters.default,
        // Example displayTransform: underline all non-spaces
        displayTransform = (buffer, cursor) => {

          def hl(b: Vector[Char]): Vector[Char] = b.flatMap{
            case ' ' => " "
            case '\n' => "\n"
            case c => Console.UNDERLINED + c + Console.RESET

          }
          val buffer2 = selection.mark match{
            case None => hl(buffer)
            case Some(mark) =>
              val Seq(min, max) = Seq(mark, cursor).sorted
              val (a, b0) = buffer.splitAt(min)
              val (b, c) = b0.splitAt(max - min)
              hl(a) ++ Console.REVERSED ++ b ++ Console.RESET ++ hl(c)
          }
          (buffer2, cursor)
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

