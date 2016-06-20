package ammonite.repl.session

import ammonite.repl.TestRepl
import ammonite.repl.TestUtils._
import ammonite.repl.tools.IvyThing
import utest._

import scala.collection.{immutable => imm}
import scala.util.Properties

object ImportHookTests extends TestSuite{

  val tests = TestSuite{
    println("ImportHookTests")
    val check = new TestRepl()
    'file{
      'basic - check.session("""
        @ import $file.repl.src.test.resources.importHooks.Basic.basicValue

        @ basicValue
        res1: Int = 31337
      """)

      'inline - check.session("""
        @ import $file.repl.src.test.resources.importHooks.Basic, Basic.basicValue

        @ basicValue
        res1: Int = 31337
      """)
      'partiallyQualified - check.session("""
        @ import $file.repl.src.test.resources.importHooks.Basic

        @ Basic.basicValue
        res1: Int = 31337
      """)

    }
    'ivy{
      'basic - check.session("""
        @ import scalatags.Text.all._
        error: not found: value scalatags

        @ import $ivy.`com.lihaoyi::scalatags:0.5.3`

        @ import scalatags.Text.all._

        @ div("Hello").render
        res2: String = "<div>Hello</div>"
      """)

      'explicitBinaryVersion - check.session(s"""
        @ import scalatags.Text.all._
        error: not found: value scalatags

        @ import $$ivy.`com.lihaoyi:scalatags_${IvyThing.scalaBinaryVersion}:0.5.3`

        @ import scalatags.Text.all._

        @ div("Hello").render
        res2: String = "<div>Hello</div>"
      """)

      'inline - check.session("""
        @ import scalatags.Text.all._
        error: not found: value scalatags

        @ import $ivy.`com.lihaoyi::scalatags:0.5.3`, scalatags.Text.all._

        @ div("Hello").render
        res1: String = "<div>Hello</div>"
      """)
    }
    'url{
      val scriptUrl = "https://raw.githubusercontent.com/lihaoyi/Ammonite/master/repl/src/test/resources/scripts/Annotation.scala"
      'basic - check.session(s"""
        @ import $$url.`$scriptUrl`
        error: $$url import failed

        @ import $$url.{`$scriptUrl` => remote}

        @ remote.product(1, List(2, 3, 4))
        res1: Int = 24
      """)
      'inline - check.session(s"""
        @ import $$url.`$scriptUrl`
        error: $$url import failed

        @ import $$url.{`$scriptUrl` => remote}; remote.product(1, List(2, 3, 4))
        import $$url.{`$scriptUrl` => remote};
        res0_1: Int = 24
      """)
    }
  }
}
