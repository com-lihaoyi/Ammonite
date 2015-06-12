package ammonite.terminal

import acyclic.file
import scala.annotation.tailrec



// Test Unicode:  漢語;𩶘da
object Term{
  def main(args: Array[String]): Unit = {
    var history = List.empty[String]
    rec()
    @tailrec def rec(): Unit = {
      TermCore.readLine(
        "@ ",
        System.in,
        System.out,
        new AdvancedFilters.SelectionFilter orElse
        AdvancedFilters.advancedNavFilter orElse
        ReadlineFilters.navFilter orElse
        new ReadlineFilters.cutPasteFilter() orElse
        // Example multiline support by intercepting Enter key
        new ReadlineFilters.HistoryFilter(history) orElse
        BasicFilters.multilineFilter orElse
        BasicFilters.default,
        // Example displayTransform: underline all non-spaces
        displayTransform = (buffer, cursor) => {
          val buffer2 = buffer.flatMap{
            case ' ' => " "
            case '\n' => "\n"
            case c => Console.UNDERLINED + c + Console.RESET
          }
          (buffer2 , cursor)
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

