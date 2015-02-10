package ammonite.repl

import utest._

import scala.collection.{immutable => imm}

object AdvancedTests extends TestSuite{
  val tests = TestSuite{
    val check = new Checker()
    'loadIvy{
      check.fail("import scalatags.Text.all._", _.contains("not found: value scalatags"))
      check("""load.ivy("com.lihaoyi", "scalatags_2.11", "0.4.5")""")
      check("import scalatags.Text.all._", "import scalatags.Text.all._")
      check(
        """a("omg", href:="www.google.com").render""",
        """res3: String = "<a href=\"www.google.com\">omg</a>""""
      )
    }
  }
}
