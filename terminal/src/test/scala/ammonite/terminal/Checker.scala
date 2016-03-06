package ammonite.terminal

import ammonite.terminal.filters.{GUILikeFilters, BasicFilters, ReadlineFilters}
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
  var currentMsg = ""
  Predef.assert(
    currentCursor != -1,
    "Cannot find `_` in grid passed to Checker, it needs to " +
    "exist to tell the checker where the cursor starts off at"
  )
  def runMsg(actions: TermCore.MsgAction*) = {
    val (endGrid, endCursor, endMsg) = actions.foldLeft((currentGrid, currentCursor, "")) {
      case ((g, c, m0), f) =>
        val (g1, c1, msg) = f(g, c)
        (g1, math.min(g1.length, math.max(0, c1)), msg)
    }
    currentGrid = endGrid
    currentCursor = endCursor
    currentMsg = endMsg
    this
  }
  def run(actions: TermCore.Action*) = {
    runMsg(actions.map{ f => (b: Vector[Char], c: Int) =>
      val (b1, c1) = f(b, c)
      (b1, c1, "")
    }:_*)
  }
  def stringGrid = {
    val prefix = currentGrid.take(currentCursor)
    val suffix = currentGrid.drop(currentCursor)
    val middle = "_"

    (prefix ++ middle ++ suffix).mkString
  }
  def check(end0: String) = {
    val expectedStringGrid = Checker.normalize(end0)
    val actualGrid = stringGrid
    assert(actualGrid == expectedStringGrid)
    this
  }
  def checkMsg(expectedMsg: String) = {
    assert(currentMsg == expectedMsg)
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
