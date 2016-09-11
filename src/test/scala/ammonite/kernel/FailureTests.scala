package ammonite.kernel

import org.scalatest.FreeSpec
import KernelTests._
import kernel._
import scalaz._

class FailureTests extends FreeSpec {

  "compileFailure" in {
    checkFailure(
      Vector(
        (s"$previousIden", {
          case NonEmptyList(h, tl) => tl.isEmpty && h.msg.contains("not found: value _it")
        }),
        ("java", {
          case NonEmptyList(h, tl) => tl.isEmpty && h.msg.contains("package java is not a value")
        }),
        ("1 + vale", {
          case NonEmptyList(h, tl) => tl.isEmpty && h.msg.contains("not found: value vale")
        }),
        ("1 + oogachaka; life; math.sqrt(true)", {
          case x =>
            (x.size == 3) && {
              val checks: NonEmptyList[String => Boolean] = NonEmptyList(_.contains("not found: value oogachaka"),
                                                                         _.contains("not found: value life"),
                                                                         _.contains("type mismatch"))
              val zipped = x.zip(checks)
              zipped match {
                case NonEmptyList((err, fn), tl) =>
                  tl.foldLeft(fn(err.msg)) {
                    case (res, (errx, fnx)) => res && (fnx(errx.msg))
                  }
              }
            }
        })
      ))
  }

  // "compilerCrash" in {
  //   // Make sure compiler crashes provide the appropiate error
  //   // messaging, and the REPL continues functioning after
  //   check.session("""
  //       @ val x = 1
  //       x: Int = 1

  //       @ /* trigger compiler crash */ trait Bar { super[Object].hashCode }
  //       error: java.lang.AssertionError: assertion failed

  //       @ 1 + x
  //       res1: Int = 2
  //     """)
  // }

  // "ivyFail" in {
  //   check.session("""
  //       @ import $ivy.`com.lihaoyi::upickle:0.1.12312-DOESNT-EXIST`
  //       error: failed to resolve ivy dependencies
  //     """)
  // }

  // "exceptionHandling" in {
  //   check.fail("""throw new Exception("lol", new Exception("hoho"))""",
  //              x =>
  //                // It contains the things we want
  //                x.contains("java.lang.Exception: lol") &&
  //                  x.contains("java.lang.Exception: hoho") &&
  //                  // and none of the stuff we don't want
  //                  x.lines.length == 6 &&
  //                  !x.contains("Something unexpected went wrong =("))
  // }

}
