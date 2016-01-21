package ammonite.terminal

import utest._

object HistoryFilterTests extends TestSuite {

  val tests = TestSuite {
    val rest = LazyList.continually(1) // Ignore.
    val assertCommand =
      """(.*)<([ud]+)>(.*)""".r

    'history {
      var history = List.empty[String]

      /**
        * Runs terminal session and asserts on lines matching assertCommand.
        *
        * - A line ending with ¶ means the user pressed enter.
        * - A line not ending with ¶ means the user edited the line.
        */
      def check(session: String): Unit = {
        history = List.empty[String]
        var assertsSomething = false
        val commands = session.trim.split("¶")
        commands.foreach { command =>
          val filter = ReadlineFilters.HistoryFilter(() => history)
          val toExecute = command.lines.map(_.trim).withFilter(!_.isEmpty)
            .map {
              case assertCommand(prefix, upDownSequence, expected) =>
                val b = prefix.toVector
                upDownSequence.foreach {
                  case 'd' =>
                    filter.nextHistory(b, rest)
                  case 'u' =>
                    filter.previousHistory(b, rest)
                }
                val result = filter.lastCommand.mkString
                assert(result == expected)
                assertsSomething = true
                result
              case cmd => cmd
            }
          val exec = toExecute.toList.last
          history = exec :: history
        }
        assert(assertsSomething)
      }

      'empty - {
        "up down" - check(
          """
          a¶
          <ud>¶
          """)


        "up up down" - check(
          """
          a¶
          b¶
          <uud>b¶
          """)

        "up up" - check(
          """
          a¶
          b¶
          <uu>a¶
          """)

        'up - check(
          """
          a¶
          <u>a¶
          """)
      }

      'fallback - {
        'bottom - check(
          """
          a¶
          b¶
          <dddduudddddddddddddddddd>¶
          """)

        'top - check(
          """
          a¶
          b¶
          <uuuuddduuuuuuuuuuuuuuuuuu>a¶
          """)
      }

      'duplicates - {
        * - check(
          """
          banana¶
          a¶
          a¶
          <uu>banana¶
          """)
        * - check(
          """
          abc¶
          ab¶
          ab¶
          a¶
          a<uu>abc¶
          """)
      }

      'prefix - {
        * - check(
          """
          11¶
          aa¶
          1<u>11¶
          """)

        * - check(
          """
          123¶
          12¶
          aaa¶
          1<u>12¶
          1<uu>123¶
          """)

        * - check(
          """
          println("foo")¶
          val x = 1¶
          val y = 2¶
          p<u>println("foo")¶
          v<u>val y = 2¶
          val x<u>val x = 1¶
          """)
      }

      'edits - {
        'abc - check(
          """
          abcd¶
          abc¶
          ab¶
          a¶
          a<u>ab
          abc<u>abcd¶
          """)
      }
    }
  }
}

