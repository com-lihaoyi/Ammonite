package ammonite.terminal

import utest._

import scala.collection.{immutable => imm}

object NavigationTests extends TestSuite{
  def normalize(s: String) = {
    val lines = s.lines.toVector
    val min = lines.map(_.indexWhere(_ != ' '))
      .filter(_ != -1)
      .min
    lines.drop(1).dropRight(1).map(_.drop(min)).mkString("\n")

  }
  def check(grid0: String,
            start0: String,
            end0: String,
            actions: (Int => Int)*) = {


    val grid = normalize(grid0)
    val start = normalize(start0)
    val end = normalize(end0)

    val startCursor = start.indexOf('_')
    val endCursor = actions.foldLeft(startCursor)(
      (a, f) => math.min(grid.length, math.max(0, f(a)))
    )

    val endState =
      if (endCursor == grid.length) grid + '_'
      else if (grid(endCursor) != '\n') grid.updated(endCursor, '_')
      else{
        val (a, b) = grid.splitAt(endCursor)
        a + '_' + b
      }

    println(startCursor + " -> " + endCursor)
    assert(end == endState)
  }
  val tests = TestSuite{
    'simple{
      val width = 5
      val grid =
        """
        abcd
        efgh
        ijkl
        """
      val gridv = normalize(grid).toVector

      val start =
        """
        abcd
        e_gh
        ijkl
        """

      def check(end: String, actions: (Int => Int)*) = {
        NavigationTests.check(grid, start, end, actions:_*)
      }
      'noop - check(
        """
        abcd
        e_gh
        ijkl
        """,
        x => x
      )
      'upsAndDowns{

        'down - check(
          """
          abcd
          efgh
          i_kl
          """,
          Term.moveDown(gridv, _, width)
        )
        'up - check(
          """
          a_cd
          efgh
          ijkl
          """,
          Term.moveUp(gridv, _, width)
        )
        'updown - check(
          """
          abcd
          e_gh
          ijkl
          """,
          Term.moveUp(gridv, _, width),
          Term.moveDown(gridv, _, width)
        )
        'upup - check(
          """
          _bcd
          efgh
          ijkl
          """,
          Term.moveUp(gridv, _, width),
          Term.moveUp(gridv, _, width)
        )
        'downdown- check(
          """
          abcd
          efgh
          ijkl_
          """,
          Term.moveDown(gridv, _, width),
          Term.moveDown(gridv, _, width)
        )
        'upupdown - check(
          """
          abcd
          _fgh
          ijkl
          """,
          Term.moveUp(gridv, _, width),
          Term.moveUp(gridv, _, width),
          Term.moveDown(gridv, _, width)
        )
        'downdownup - check(
          """
          abcd
          efgh_
          ijkl
          """,
          Term.moveDown(gridv, _, width),
          Term.moveDown(gridv, _, width),
          Term.moveUp(gridv, _, width)
        )
      }
      'startEnd{
        'end - check(
          """
          abcd
          efgh_
          ijkl
          """,
          Term.moveStartEnd(gridv, _, 1)
        )
        'start - check(
          """
          abcd
          _fgh
          ijkl
          """,
          Term.moveStartEnd(gridv, _, 0)
        )
      }
    }
  }
}
