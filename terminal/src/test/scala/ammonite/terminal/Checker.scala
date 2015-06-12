package ammonite.terminal
import utest._
object Checker{
  def normalize(s: String) = {
    val lines = s.lines.toVector
    val min = lines.map(_.indexWhere(_ != ' '))
      .filter(_ != -1)
      .min
    lines.drop(1).dropRight(1).map(_.drop(min)).mkString("\n").replace("\\\n", "")

  }

  def apply(width: Int, grid: String, start: String) =
    new Checker(width, normalize(grid), normalize(start))
}

/**
 * A shell emulator that you can use to test out sequences of various
 * [[TermCore.Action]]s against an in-memory (Vector[Char], Int) and
 * verify that they do the right thing.
 */
class Checker(width: Int, grid: String, start: String){

  def apply(end0: String, actions: TermCore.Action*) = {

    val end = Checker.normalize(end0)
    val gridv = grid.toVector
    val startCursor = start.indexOf('_')
    val (endGrid, endCursor) = actions.foldLeft((gridv, startCursor)) {
      case ((g, c), f) =>
        val (g1, c1) = f(g, c)
        (g1, math.min(gridv.length, math.max(0, c1)))
    }

    val endState =
      if (endCursor == endGrid.length) endGrid ++ "_"
      else if (endGrid(endCursor) != '\n') endGrid.updated(endCursor, '_')
      else{
        val (a, b) = endGrid.splitAt(endCursor)
        a ++ "_" ++ b
      }


    val endString = endState.mkString
    assert(end == endString)
  }
  val edit = new ReadlineFilters.cutPasteFilter()
  val down: TermCore.Action = BasicFilters.moveDown(_, _, width)
  val up: TermCore.Action = BasicFilters.moveUp(_, _, width)
  val home: TermCore.Action = BasicFilters.moveStart(_, _, width)
  val end: TermCore.Action = BasicFilters.moveEnd(_, _, width)
  val wordLeft: TermCore.Action = ReadlineFilters.wordLeft
  val wordRight: TermCore.Action = ReadlineFilters.wordRight
}
