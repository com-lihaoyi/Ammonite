package ammonite.terminal

import utest._
import acyclic.file

object HistoryFilterTests extends TestSuite {

  val tests = TestSuite {
    val rest = LazyList.continually(1) // Doesn't matter.
    val assertCommand = """(.*)<([ud]+)>(.*)""".r

    'selection{
      var history = List.empty[String]
      def withHistoryFilter(f: ReadlineFilters.HistoryFilter => Unit): String = {
        // Initialize new filter for each line.
        val filter = ReadlineFilters.HistoryFilter(() => history)
        f(filter)
        filter.lastHistory.mkString
      }

      def check(session: String): Unit = {
        var assertsSomething = false
        val commands = session.lines.map(_.trim).withFilter(!_.isEmpty)
        commands.foreach {
          case assertCommand(before, upDownSequence, expected) =>
            val b = before.toCharArray.toVector
            val result = withHistoryFilter { filter =>
              upDownSequence.foreach {
                case 'd' =>
                  filter.nextHistory(b, rest)
                case 'u' =>
                  filter.previousHistory(b, rest)
              }
            }
            assert(result == expected)
            assertsSomething = true
            history = result :: history
          case cmd =>
            history = cmd :: history
        }
        assert(assertsSomething)
      }

      'up - check(
        """
        a
        <u>a
        """)

      'updown - check(
        """
        a
        <ud>
        """)

      'upup - check(
        """
        a
        b
        <uu>a
        """)

      "up to the top" - check(
        """
        a
        b
        <uuuuddduuuuuuuuuuuuuuuuuu>a
        """)

      "down to the bottom" - check(
        """
        a
        b
        foo<uudddddddddddddddddddddd>foo
        """)


      'upupdown - check(
        """
        a
        b
        <uud>b
        """)

      'duplicates - check(
        """
        banana
        a
        a
        <uu>banana
        """)

      'prefix - check(
        """
        11
        aa
        1<u>11
        """)

      "prefix order" - check(
        """
        123
        12
        aaa
        1<u>12
        1<uu>123
        """)


      'prefixes - check(
        """
        println("foo")
        val x = 1
        val y = 2
        p<u>println("foo")
        v<u>val y = 2
        val x<u>val x = 1
        """)
    }
  }
}

