// package ammonite.session

// import ammonite.TestRepl
// import ammonite.runtime.tools.IvyThing
// import org.scalatest.FreeSpec

// class ImportHookTests extends FreeSpec {

//   def check = new TestRepl()

//   "repl" - {
//     "file" - {
//       "basic" in check.session("""
//           @ import $file.src.test.resources.importHooks.Basic

//           @ Basic.basicValue
//           res1: Int = 31337
//         """)

//       "inline" in check.session("""
//           @ import $file.src.test.resources.importHooks.Basic, Basic.basicValue

//           @ basicValue
//           res1: Int = 31337
//         """)

//       "partiallyQualified" in check.session("""
//           @ import $file.src.test.resources.importHooks.Basic

//           @ Basic.basicValue
//           res1: Int = 31337
//         """)

//       "multiImport" in check.session("""
//           @ import $file.src.test.resources.importHooks.{Basic, BasicTwo}

//           @ Basic.basicValue
//           res1: Int = 31337

//           @ BasicTwo.basicValueTwo
//           res2: Int = 1337
//         """)

//       "rename" in check.session("""
//           @ import $file.src.test.resources.importHooks.{Basic, BasicTwo => BasicToo}

//           @ Basic.basicValue
//           res1: Int = 31337

//           @ BasicToo.basicValueTwo
//           res2: Int = 1337
//         """)

//       "deep" in check.session("""
//           @ import $file.src.test.resources.importHooks.Deep.DeepObject.DeepInner.deepValue
//           error: Cannot resolve $file import
//         """)

//       "deepRenamed" in check.session("""
//           @ import $file.src.test.resources.importHooks.Deep.{DeepObject => DeepRenamed}
//           error: Cannot resolve $file import
//          """)
//     }

//     "ivy" - {
//       "basic" in {
//         check.session("""
//             @ import scalatags.Text.all._
//             error: not found: value scalatags

//             @ import $ivy.`com.lihaoyi::scalatags:0.5.3`

//             @ import scalatags.Text.all._

//             @ div("Hello").render
//             res2: String = "<div>Hello</div>"
//            """)
//       }

//       "explicitBinaryVersion" in {
//         check.session(s"""
//             @ import scalatags.Text.all._
//             error: not found: value scalatags

//             @ import $$ivy.`com.lihaoyi:scalatags_${IvyThing.scalaBinaryVersion}:0.5.3`

//             @ import scalatags.Text.all._

//             @ div("Hello").render
//             res2: String = "<div>Hello</div>"
//            """)
//       }

//       "inline" in {
//         check.session("""
//             @ import scalatags.Text.all._
//             error: not found: value scalatags

//             @ import $ivy.`com.lihaoyi::scalatags:0.5.3`, scalatags.Text.all._

//             @ div("Hello").render
//             res1: String = "<div>Hello</div>"
//            """)
//       }
//     }
//     // 'url{
//     //   val scriptUrl =
//     //     "https://raw.githubusercontent.com/lihaoyi/Ammonite/" +
//     //     "master/amm/src/test/resources/scripts/Annotation.sc"
//     //   'basic - {
//     //     check.session(s"""
//     //     @ import $$url.`$scriptUrl`
//     //     error: $$url import failed

//     //     @ import $$url.{`$scriptUrl` => remote}

//     //     @ remote.product(1, List(2, 3, 4))
//     //     res1: Int = 24
//     //   """)
//     //   }
//     //   'inline - {
//     //     check.session(s"""
//     //     @ import $$url.`$scriptUrl`
//     //     error: $$url import failed

//     //     @ import $$url.{`$scriptUrl` => remote}; val x = remote.product(1, List(2, 3, 4))

//     //     @ x
//     //     res1: Int = 24
//     //   """)
//     //   }
//     // }
//   }

//   "scripts" - {
//     "file" in check.session("""
//         @ import $file.src.test.resources.importHooks.FileImport

//         @ FileImport.fileImportVal
//         res1: Int = 31338
//        """)

//     "indirectFile" in check.session("""
//         @ import $file.src.test.resources.importHooks.IndirectFileImport

//         @ IndirectFileImport.indirectFileImportVal
//         res1: Int = 31339
//        """)

//     "ivy" in {
//       check.session("""
//           @ import $file.src.test.resources.importHooks.IvyImport

//           @ IvyImport.rendered
//           res1: String = "<div>Moo</div>"
//          """)
//     }

//     "deepImport" in check.session("""
//         @ import $file.src.test.resources.importHooks.DeepImport.deepValueImported
//         error: Cannot resolve $file import

//         @ import $file.src.test.resources.importHooks.DeepImport,DeepImport.deepValueImported

//         @ deepValueImported
//         res1: String = "deeeep"
//       """)
//   }

// }
