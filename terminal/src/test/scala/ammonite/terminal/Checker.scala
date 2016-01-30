package ammonite.terminal
import utest._
object Checker{
  def normalize(s: String) = {
    // Only do line/margin mangling for multi-line strings
    if (s.indexOf('\n') == -1) s
    else{
      val lines = s.lines.toVector
      val min = lines.map(_.indexWhere(_ != ' '))
        .filter(_ != -1)
        .min
      lines.drop(1).dropRight(1).map(_.drop(min)).mkString("\n").replace("\\\n", "")

    }
  }

  def apply(width: Int, grid: String) =
    new Checker(width, normalize(grid))
}

/**
 * A shell emulator that you can use to test out sequences of various
 * [[TermCore.Action]]s against an in-memory (Vector[Char], Int) and
 * verify that they do the right thing.
 */
class Checker(width: Int, grid: String){
  override def toString = s"Checker($stringGrid)"
  var currentGrid = grid.replace("_", "").toVector
  var currentCursor = grid.indexOf('_')

  def run(actions: TermCore.Action*) = {
    println("------------------------------------------")
    val (endGrid, endCursor) = actions.foldLeft((currentGrid, currentCursor)) {
      case ((g, c), f) =>
        val (g1, c1) = f(g, c)
        (g1, math.min(g1.length, math.max(0, c1)))
    }

    currentGrid = endGrid
    currentCursor = endCursor

    this
  }
  def stringGrid = {
    val prefix = currentGrid.take(currentCursor)
    val suffix = currentGrid.drop(currentCursor+1)
    val middle = currentGrid.lift(currentCursor) match{
      case None => Seq('_')
      case Some('\n') => Seq('_', '\n')
      case _ => Seq('_')
    }

    (prefix ++ middle ++ suffix).mkString
  }
  def check(end0: String) = {
    val expectedStringGrid = Checker.normalize(end0)
    val actualGrid = stringGrid
    assert(actualGrid == expectedStringGrid)
    this
  }
  def apply(end0: String, actions: TermCore.Action*) = {
    run(actions:_*)
    check(end0)
  }
  val edit = new ReadlineFilters.CutPasteFilter()
  val down: TermCore.Action = BasicFilters.moveDown(_, _, width)
  val up: TermCore.Action = BasicFilters.moveUp(_, _, width)
  val home: TermCore.Action = BasicFilters.moveStart(_, _, width)
  val end: TermCore.Action = BasicFilters.moveEnd(_, _, width)
  val wordLeft: TermCore.Action = GUILikeFilters.wordLeft
  val wordRight: TermCore.Action = GUILikeFilters.wordRight
}
