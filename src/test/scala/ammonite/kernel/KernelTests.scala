package ammonite.kernel

import ammonite.runtime.Storage
//import ammonite.repl.Repl
import ammonite.util._
import scalaz.{Name => _, _}
import kernel._
import org.scalatest.Assertions._

//import scala.collection.mutable

object KernelTests {

  private val defaultPredef = Seq(
    Name("defaultPredef") -> ammonite.main.Defaults.predefString
  )

  def buildKernel(predefs: Seq[(Name, String)] = defaultPredef) = new ReplKernel(new Storage.InMemory, predefs)

  def check(kernel: ReplKernel, checks: Vector[(String, KernelOutput => Boolean)]) = {
    val (res, idx) = checks.zipWithIndex.foldLeft((true, -1)) {
      case ((res, resIdx), ((code, opTest), idx)) => {
        if (res) {
          val currRes = opTest(kernel.process(code))
          if (currRes) {
            (currRes, -1)
          } else {
            (currRes, idx)
          }
        } else {
          (res, resIdx)
        }
      }
    }
    val msg = if (idx != -1) s"failed for input: ${checks(idx)._1}"
    assert(res, msg)
  }

  def checkSuccess(kernel: ReplKernel, checks: Vector[(String, Any => Boolean)]) = {
    val modifiedChecks: Vector[(String, KernelOutput => Boolean)] = checks map {
      case (code, fn) =>
        val modified: KernelOutput => Boolean = {
          case Some(Success((_, x))) => fn(x)
          case _ => false
        }
        (code, modified)
    }
    check(kernel, modifiedChecks)
  }

  def checkFailure(kernel: ReplKernel, checks: Vector[(String, NonEmptyList[LogError] => Boolean)]) = {
    val modifiedChecks: Vector[(String, KernelOutput => Boolean)] = checks map {
      case (code, fn) =>
        val modified: KernelOutput => Boolean = {
          case Some(Failure(errors)) => fn(errors)
          case _ => false
        }
        (code, modified)
    }
    check(kernel, modifiedChecks)
  }
//   def session(sess: String): Unit = {
//     // Remove the margin from the block and break
//     // it into blank-line-delimited steps
//     val margin = sess.lines.filter(_.trim != "").map(_.takeWhile(_ == ' ').length).min
//     // Strip margin & whitespace

//     val steps = sess.replace(Util.newLine + margin, Util.newLine).replaceAll(" *\n", "\n").split("\n\n")

//     for ((step, index) <- steps.zipWithIndex) {
//       // Break the step into the command lines, starting with @,
//       // and the result lines
//       val (cmdLines, resultLines) = step.lines.map(_.drop(margin)).partition(_.startsWith("@"))

//       val commandText = cmdLines.map(_.stripPrefix("@ ")).toVector

//       // Make sure all non-empty, non-complete command-line-fragments
//       // are considered incomplete during the parse
//       for (incomplete <- commandText.inits.toSeq.drop(1).dropRight(1)) {
//         assert(ammonite.runtime.Parsers.split(incomplete.mkString(Util.newLine)) == None)
//       }

//       // Finally, actually run the complete command text through the
//       // interpreter and make sure the output is what we expect
//       val expected = resultLines.mkString(Util.newLine).trim
//       allOutput += commandText.map(Util.newLine + "@ " + _).mkString(Util.newLine)

//       val (processed, out, warning, error, info) = run(commandText.mkString(Util.newLine))
// //      kernel.interp.handleOutput(processed)

//       if (expected.startsWith("error: ")) {
//         val strippedExpected = expected.stripPrefix("error: ")
//         assert(error.contains(strippedExpected))

//       } else if (expected.startsWith("warning: ")) {
//         val strippedExpected = expected.stripPrefix("warning: ")
//         assert(warning.contains(strippedExpected))

//       } else if (expected.startsWith("info: ")) {
//         val strippedExpected = expected.stripPrefix("info: ")
//         assert(info.contains(strippedExpected))

//       } else if (expected == "") {
//         processed match {
//           case Res.Success(_) => // do nothing
//           case Res.Skip => // do nothing
//           case _: Res.Failing =>
//             println(identity(error))
//             println(identity(warning))
//             println(identity(out))
//             println(identity(info))
//             assert {
//               identity(error)
//               identity(warning)
//               identity(out)
//               identity(info)
//               false
//             }
//         }

//       } else {
//         processed match {
//           case Res.Success(str) =>
//             // Strip trailing whitespace
//             def normalize(s: String) = s.lines.map(_.replaceAll(" *$", "")).mkString(Util.newLine).trim()
//             failLoudly(
//               assert {
//                 identity(error)
//                 identity(warning)
//                 identity(info)
//                 normalize(out) == normalize(expected)
//               }
//             )

//           case Res.Failure(ex, failureMsg) =>
//             assert {
//               identity(error)
//               identity(warning)
//               identity(out)
//               identity(info)
//               identity(expected)
//               false
//             }
//           case Res.Exception(ex, failureMsg) =>
//             val trace = Repl.showException(
//                 ex,
//                 fansi.Attrs.Empty,
//                 fansi.Attrs.Empty,
//                 fansi.Attrs.Empty
//               ) + Util.newLine + failureMsg
//             assert({ identity(trace); identity(expected); false })
//           case _ =>
//             throw new Exception(
//               s"Printed $out does not match what was expected: $expected"
//             )
//         }
//       }
//     }
//   }

  // def run(input: String) = {
  //   kernel.process(input)
  //   // processed match {
  //   //   case Res.Failure(ex, s) => printer.error(s)
  //   //   case Res.Exception(throwable, msg) =>
  //   //     printer.error(
  //   //       Repl.showException(throwable, fansi.Attrs.Empty, fansi.Attrs.Empty, fansi.Attrs.Empty)
  //   //     )
  //   //   case _ =>
  //   // }
  //   // (
  //   //   processed,
  //   //   outBuffer.mkString,
  //   //   warningBuffer.mkString(Util.newLine),
  //   //   errorBuffer.mkString(Util.newLine),
  //   //   infoBuffer.mkString(Util.newLine)
  //   // )
  // }

  // def fail(input: String, failureCheck: String => Boolean = _ => true) = {
  //   val (processed, _, _, _, _) = run(input)

  //   processed match {
  //     case Res.Success(v) =>
  //       assert({ identity(v); identity(allOutput); false })
  //     case Res.Failure(ex, s) =>
  //       failLoudly(assert(failureCheck(s)))
  //     case Res.Exception(ex, s) =>
  //       val msg = Repl.showException(
  //           ex,
  //           fansi.Attrs.Empty,
  //           fansi.Attrs.Empty,
  //           fansi.Attrs.Empty
  //         ) + Util.newLine + s
  //       failLoudly(assert(failureCheck(msg)))
  //     case _ => ???
  //   }
  // }

  // def result(input: String, expected: Res[Evaluated]) = {
  //   val (processed, _, _, _, _) = run(input)
  //   assert(processed == expected)
  // }
  // def failLoudly[T](t: => T) =
  //   try t
  //   catch {
  //     case e: Throwable =>
  //       println("FAILURE TRACE" + Util.newLine + allOutput)
  //       throw e
  //   }

}
