package ammonite.terminal

import utest._

import scala.collection.{immutable => imm}

object NavigationTests extends TestSuite{
  def normalize(s: String) = {
    val lines = s.lines.toVector
    val min = lines.map(_.indexWhere(_ != ' '))
      .filter(_ != -1)
      .min
    lines.drop(1).map(_.drop(min)).mkString("\n")

  }
  def check(grid0: String,
            start0: String,
            end0: String,
            action: Int => Int) = {


    val grid = normalize(grid0)
    val start = normalize(start0)
    val end = normalize(end0)

    val startCursor = start.indexOf('_')
    val endCursor = action(startCursor)
    val endState = grid.updated(endCursor, '_')
    println(startCursor + " -> " + endCursor)
    assert(end == endState)
  }
  val tests = TestSuite{
    'test{

      val width = 5
      val grid =
        """
        abcd
        efgh
        ijkl
        """
      val start =
        """
        abcd
        e_gh
        ijkl
        """

      def check(action: Int => Int, end: String) = {
        NavigationTests.check(grid, start, end, action)
      }

      * - check(
        x => x,
        """
        abcd
        e_gh
        ijkl
        """
      )
      * - check(
        Term.moveDown(normalize(grid).toVector, _, width),
        """
        abcd
        efgh
        i_kl
        """
      )
      * - check(
        Term.moveUp(normalize(grid).toVector, _, width),
        """
        a_cd
        efgh
        ijkl
        """
      )
    }


  }
}
