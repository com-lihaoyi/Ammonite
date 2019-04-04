package ammonite.session

import ammonite.DualTestRepl
import ammonite.TestUtils._
import ammonite.runtime.tools.IvyThing
import utest._

import scala.collection.{immutable => imm}
import scala.util.Properties
import ammonite.util.Util

object ImportHookTests extends TestSuite{

  val tests = Tests{
    println("ImportHookTests")
    val check = new DualTestRepl()
    'repl{
      'file{
        'basic - check.session("""
          @ import $file.amm.src.test.resources.importHooks.Basic

          @ Basic.basicValue
          res1: Int = 31337
        """)

        'inline - check.session("""
          @ import $file.amm.src.test.resources.importHooks.Basic, Basic.basicValue

          @ basicValue
          res1: Int = 31337
        """)

        'partiallyQualified - check.session("""
          @ import $file.amm.src.test.resources.importHooks.Basic

          @ Basic.basicValue
          res1: Int = 31337
        """)

        'multiImport - check.session("""
          @ import $file.amm.src.test.resources.importHooks.{Basic, BasicTwo}

          @ Basic.basicValue
          res1: Int = 31337

          @ BasicTwo.basicValueTwo
          res2: Int = 1337
        """)

        'rename - check.session("""
          @ import $file.amm.src.test.resources.importHooks.{Basic, BasicTwo => BasicToo}

          @ Basic.basicValue
          res1: Int = 31337

          @ BasicToo.basicValueTwo
          res2: Int = 1337
        """)

        'deep - check.session("""
          @ import $file.amm.src.test.resources.importHooks.Deep.DeepObject.DeepInner.deepValue
          error: Cannot resolve $file import
        """)


        'deepRenamed - check.session("""
          @ import $file.amm.src.test.resources.importHooks.Deep.{DeepObject => DeepRenamed}
          error: Cannot resolve $file import
         """)

      }
      'ivy{
        'basic - {
          check.session("""
            @ import scalatags.Text.all._
            error: not found: value scalatags

            @ import $ivy.`com.lihaoyi::scalatags:0.6.8`

            @ import scalatags.Text.all._

            @ div("Hello").render
            res2: String = "<div>Hello</div>"
           """)
        }

        'explicitBinaryVersion - {
          check.session(s"""
            @ import scalatags.Text.all._
            error: not found: value scalatags

            @ import $$ivy.`com.lihaoyi:scalatags_${IvyThing.scalaBinaryVersion}:0.6.8`

            @ import scalatags.Text.all._

            @ div("Hello").render
            res2: String = "<div>Hello</div>"
           """)
        }

        'inline - {
          check.session("""
            @ import scalatags.Text.all._
            error: not found: value scalatags

            @ import $ivy.`com.lihaoyi::scalatags:0.6.8`, scalatags.Text.all._

            @ div("Hello").render
            res1: String = "<div>Hello</div>"
           """)
        }

        'inlineFull - {
          // no more macroparadise in 2.13
          if (scala2_11 || scala2_12) check.session("""
            @ import org.scalamacros.paradise.Settings._
            error: object scalamacros is not a member of package org

            @ import $ivy.`org.scalamacros:::paradise:2.1.0`, org.scalamacros.paradise.Settings._

            @ boolSetting("key").value
            res1: Boolean = false
           """)
        }
      }
      'url{
        val scriptUrl =
          "https://raw.githubusercontent.com/lihaoyi/Ammonite/" +
          "master/amm/src/test/resources/scripts/Annotation.sc"
        'basic - {
          check.session(s"""
          @ import $$url.`$scriptUrl`
          error: $$url import failed

          @ import $$url.{`$scriptUrl` => remote}

          @ remote.product(1, List(2, 3, 4))
          res1: Int = 24
        """)
        }
        'inline - {
          check.session(s"""
          @ import $$url.`$scriptUrl`
          error: $$url import failed

          @ import $$url.{`$scriptUrl` => remote}; val x = remote.product(1, List(2, 3, 4))

          @ x
          res1: Int = 24
        """)
        }
      }
    }
    'scripts{
      'file - check.session("""
        @ import $file.amm.src.test.resources.importHooks.FileImport

        @ FileImport.fileImportVal
        res1: Int = 31338
       """)

      'indirectFile - check.session("""
        @ import $file.amm.src.test.resources.importHooks.IndirectFileImport

        @ IndirectFileImport.indirectFileImportVal
        res1: Int = 31339
       """)

      'ivy - {
        check.session("""
          @ import $file.amm.src.test.resources.importHooks.IvyImport

          @ IvyImport.rendered
          res1: String = "<div>Moo</div>"
         """)
      }

      'deepImport - check.session("""
        @ import $file.amm.src.test.resources.importHooks.DeepImport.deepValueImported
        error: Cannot resolve $file import

        @ import $file.amm.src.test.resources.importHooks.DeepImport,DeepImport.deepValueImported

        @ deepValueImported
        res1: String = "deeeep"
      """)
    }
  }
}
