package ammonite.session

import ammonite.DualTestRepl
import ammonite.TestUtils._
import ammonite.interp.api.IvyConstructor
import utest._

import scala.collection.{immutable => imm}
import scala.util.Properties
import ammonite.util.Util

object ImportHookTests extends TestSuite{

  val tests = Tests{
    println("ImportHookTests")
    val check = new DualTestRepl()
    test("repl"){
      test("file"){
        test("basic") - check.session("""
          @ import $file.amm.src.test.resources.importHooks.Basic

          @ Basic.basicValue
          res1: Int = 31337
        """)

        test("inline") - check.session("""
          @ import $file.amm.src.test.resources.importHooks.Basic, Basic.basicValue

          @ basicValue
          res1: Int = 31337
        """)

        test("partiallyQualified") - check.session("""
          @ import $file.amm.src.test.resources.importHooks.Basic

          @ Basic.basicValue
          res1: Int = 31337
        """)

        test("multiImport") - check.session("""
          @ import $file.amm.src.test.resources.importHooks.{Basic, BasicTwo}

          @ Basic.basicValue
          res1: Int = 31337

          @ BasicTwo.basicValueTwo
          res2: Int = 1337
        """)

        test("rename") - check.session("""
          @ import $file.amm.src.test.resources.importHooks.{Basic, BasicTwo => BasicToo}

          @ Basic.basicValue
          res1: Int = 31337

          @ BasicToo.basicValueTwo
          res2: Int = 1337
        """)

        test("deep") - check.session("""
          @ import $file.amm.src.test.resources.importHooks.Deep.DeepObject.DeepInner.deepValue
          error: Cannot resolve $file import
        """)


        test("deepRenamed") - check.session("""
          @ import $file.amm.src.test.resources.importHooks.Deep.{DeepObject => DeepRenamed}
          error: Cannot resolve $file import
         """)

      }
      test("ivy"){
        test("basic"){
          check.session("""
            @ import scalatags.Text.all._
            error: not found: value scalatags

            @ import $ivy.`com.lihaoyi::scalatags:0.7.0`

            @ import scalatags.Text.all._

            @ div("Hello").render
            res2: String = "<div>Hello</div>"
           """)
        }

        test("explicitBinaryVersion"){
          check.session(s"""
            @ import scalatags.Text.all._
            error: not found: value scalatags

            @ import $$ivy.`com.lihaoyi:scalatags_${IvyConstructor.scalaBinaryVersion}:0.7.0`

            @ import scalatags.Text.all._

            @ div("Hello").render
            res2: String = "<div>Hello</div>"
           """)
        }

        test("inline"){
          check.session("""
            @ import scalatags.Text.all._
            error: not found: value scalatags

            @ import $ivy.`com.lihaoyi::scalatags:0.7.0`, scalatags.Text.all._

            @ div("Hello").render
            res1: String = "<div>Hello</div>"
           """)
        }

        test("inlineFull"){
          // no more macroparadise in 2.13
          if (scala2_12) check.session("""
            @ import org.scalamacros.paradise.Settings._
            error: object scalamacros is not a member of package org

            @ import $ivy.`org.scalamacros:::paradise:2.1.0`, org.scalamacros.paradise.Settings._

            @ boolSetting("key").value
            res1: Boolean = false
           """)
        }
      }
      test("url"){
        val scriptUrl =
          "https://raw.githubusercontent.com/lihaoyi/Ammonite/" +
          "master/amm/src/test/resources/scripts/Annotation.sc"
        test("basic"){
          check.session(s"""
          @ import $$url.`$scriptUrl`
          error: $$url import failed

          @ import $$url.{`$scriptUrl` => remote}

          @ remote.product(1, List(2, 3, 4))
          res1: Int = 24
        """)
        }
        test("inline"){
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
    test("scripts"){
      test("file") - check.session("""
        @ import $file.amm.src.test.resources.importHooks.FileImport

        @ FileImport.fileImportVal
        res1: Int = 31338
       """)

      test("indirectFile") - check.session("""
        @ import $file.amm.src.test.resources.importHooks.IndirectFileImport

        @ IndirectFileImport.indirectFileImportVal
        res1: Int = 31339
       """)

      test("ivy"){
        check.session("""
          @ import $file.amm.src.test.resources.importHooks.IvyImport

          @ IvyImport.rendered
          res1: String = "<div>Moo</div>"
         """)
      }

      test("deepImport") - check.session("""
        @ import $file.amm.src.test.resources.importHooks.DeepImport.deepValueImported
        error: Cannot resolve $file import

        @ import $file.amm.src.test.resources.importHooks.DeepImport,DeepImport.deepValueImported

        @ deepValueImported
        res1: String = "deeeep"
      """)
    }
  }
}
