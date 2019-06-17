package ammonite.unit

import ammonite.repl.Highlighter
import ammonite.util.Util
import utest._

object ParserTests extends TestSuite{

  val tests = Tests {
    println("ParserTests")
    test("shebang"){
      def check(original: String, expected: String) = {
        val skipped = ammonite.interp.Interpreter.skipSheBangLine(
          ammonite.util.Util.normalizeNewlines(original)
        )
        val normalizedExpected = ammonite.util.Util.normalizeNewlines(expected)
        assert(skipped == normalizedExpected)
      }
      "env" - check(
        """#! /usr/bin/env amm
          |
          |println("Hello") """.stripMargin,
        """
          |
          |println("Hello") """.stripMargin
      )
      "hardcoded" - check(
        """#! /Users/lihaoyi/Dropbox/Github/Ammonite/amm/target/amm
          |
          |println("Hello") """.stripMargin,
        """
          |
          |println("Hello") """.stripMargin
      )
      "nix-shell" - check(
        """#! /usr/bin/env nix-shell
          |#! nix-shell -i amm -p ammonite-repl
          |
          |println("Hello") """.stripMargin,
        """
          |
          |
          |println("Hello") """.stripMargin
      )
      "nix-shell-endshebang" - check(
        """#! /usr/bin/env nix-shell
          |#! nix-shell -i amm -p ammonite-repl
          |
          |!#
          |
          |println("Hello") """.stripMargin,
        """
          |
          |
          |
          |
          |println("Hello") """.stripMargin
      )
    }

    // Sanity check the logic that runs when you press ENTER in the REPL and
    // detects whether a set of input lines is...
    //
    // - Complete, and can be submitted without needing additional input
    // - Incomplete, and thus needs additional lines of input from the user
    // - Invalid, and thus can be rejected without needing additional input
    //
    // Not nearly comprehensive, but hopefully if someone really borks this
    // somewhat-subtle logic around the parsers, one of these will catch it
    test("endOfCommandDetection"){
      def assertResult(x: String, pred: Option[fastparse.Parsed[_]] => Boolean) = {
        val res = ammonite.interp.Parsers.split(x)
        assert(pred(res))
      }
      def assertIncomplete(x: String) = assertResult(x, _.isEmpty)
      def assertComplete(x: String) = assertResult(x, _.isDefined)
      def assertInvalid(x: String) =
        assertResult(x, res => res.isDefined && res.get.isInstanceOf[fastparse.Parsed.Failure])

      test("endOfCommand"){
        test - assertComplete("{}")
        test - assertComplete("foo.bar")
        test - assertComplete("foo.bar // line comment")
        test - assertComplete("foo.bar /* block comment */")
        test - assertComplete("va va") // postfix
        test - assertComplete("")
        test - assertComplete("""
          {
            val x = 1
          }
        """)
        test - assertComplete("""
          {{
            val x = 1
          }}
        """)
        test - assertComplete("""
          val r = (1 until 1000).view.filter(n => n % 3 == 0 || n % 5 == 0).sum
        """)
      }
      test("notEndOfCommand"){

        test - assertIncomplete("{")
        test - assertIncomplete("foo.bar /* incomplete block comment")
        test - assertIncomplete("""
          val r = (1 until 1000.view.filter(n => n % 3 == 0 || n % 5 == 0)
        """)
        test - assertIncomplete("""
          val r = (1 until 1000).view.filter(n => n % 3 == 0 || n % 5 == 0
        """)
        
      }
      test("commandIsBroken"){
        test - assertInvalid("}")
        test - assertInvalid("{val val ")
        test - assertInvalid("val val ")
        test - assertInvalid("""
          {
          val val
        """)
        test - assertInvalid("""
          val r = (1 until 1000).view.filter(n => n % 3 == 0 || n % 5 <- 0
        """)
        test - assertInvalid("""
          val r <- (1 until 1000).view.filter(n => n % 3 == 0 || n % 5 == 0
        """)
      }
      test("examples"){
        test("small"){
          val input = Util.normalizeNewlines(
            """ for {
                 a <- List(1);
                 b <- List(2)
              } yield (1, 2)"""
          )
          val lines = Predef.augmentString(input).lines.toVector
          for(i <- 1 until lines.length) {
            val prefix = lines.take(i).mkString(Util.newLine)
            // Only the entire input, which is the last prefix, is complete.
            // All others are incomplete
            if (i == lines.length) assertComplete(prefix)
            else assertIncomplete(prefix)
          }
        }
        test("medium"){
          val input = Util.normalizeNewlines(
            """ val ls = for(y <- 1900 to 2000; m <- 1 to 12) yield {
                  if(m == 2)
                    if (y % 4 == 0 && (y % 100 != 0 || y % 400 == 0)) 29 else 28
                  else
                    lengths(m - 1)
                } """
          )
          val lines = Predef.augmentString(input).lines.toVector
          for(i <- 1 until lines.length) {
            val prefix = lines.take(i).mkString(Util.newLine)
            // Only the entire input, which is the last prefix, is complete.
            // All others are incomplete
            if (i == lines.length) assertComplete(prefix)
            else assertIncomplete(prefix)
          }
        }
        test("big"){
          val input = Util.normalizeNewlines(
            """import play.core.server._, play.api.routing.sird._, play.api.mvc._ // 0
               import scalaj.http._                                               // 1
               val server = NettyServer.fromRouter(new ServerConfig(              // 2
                 rootDir = new java.io.File("."),                                 // 3
                 port = Some(19000), sslPort = None,                              // 4
                 address = "0.0.0.0", mode = play.api.Mode.Dev,                   // 5
                 properties = System.getProperties,                               // 6
                 configuration = play.api.Configuration(                          // 7
                   "play.server.netty" -> Map(                                    // 8
                     "maxInitialLineLength" -> 4096,                              // 9
                     "maxHeaderSize" -> 8192,                                     // 10
                     "maxChunkSize" -> 8192,                                      // 11
                     "log.wire" -> false,                                         // 12
                     "eventLoopThreads" -> 0,                                     // 13
                     "transport" -> "jdk",                                        // 14
                     "option.child" -> Map()                                      // 15
                   )                                                              // 16
                 )                                                                // 17
               )) {                                                               // 18
                 case GET(p"/hello/$to") => Action { Results.Ok(s"Hello $to") }   // 19
               }                                                                  // 20
               try {                                                              // 21
                 println(Http("http://localhost:19000/hello/bar").asString.body)  // 22
               }finally{                                                          // 23
                 server.stop()                                                    // 24
               }                                                                  // 25"""
          )
          val lines = Predef.augmentString(input).lines.toVector
          // Every line n where the prefix formed by lines 0 to n (inclusive)
          // is a valid, complete input. Every other line *should* be incomplete,
          // and no prefix in this example should be invalid
          val completeLines = Set(0, 1, 20, 25)
          for(i <- 0 until lines.length) {

            val prefix = lines.take(i + 1).mkString(Util.newLine)
            if (completeLines(i)) assertComplete(prefix)
            else assertIncomplete(prefix)
          }
        }
      }
    }
  }
}
