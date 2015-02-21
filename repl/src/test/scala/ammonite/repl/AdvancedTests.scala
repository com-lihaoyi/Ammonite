package ammonite.repl

import utest._

import scala.collection.{immutable => imm}

object AdvancedTests extends TestSuite{
  val tests = TestSuite{
    val check = new Checker()
    'load{
      'ivy{
        'standalone{
          check.fail("import scalatags.Text.all._", _.contains("not found: value scalatags"))
          check("""load.ivy("com.lihaoyi", "scalatags_2.11", "0.4.5")""")
          check("import scalatags.Text.all._", "import scalatags.Text.all._")
          check(
            """a("omg", href:="www.google.com").render""",
            """res2: String = "<a href=\"www.google.com\">omg</a>""""
          )
        }
        'dependent{
          // Make sure it automatically picks up jawn-parser
          check("""load.ivy("com.lihaoyi", "upickle_2.11", "0.2.6")""")
          check("import upickle._")
          check("upickle.write(Seq(1, 2, 3))", """res2: String = "[1,2,3]"""")
        }
      }
      'code{
        check("""load("val x = 1")""")
        check("""x""", "res2: Int = 1")
      }
    }
    'history{
      check("""val x = 1""")
      check("x")
      check("history", """res2: scala.Seq[String] = Vector("val x = 1", "x")""")
    }
  }
}
