package ammonite.kernel

import org.scalatest.FreeSpec
import KernelTests._
import kernel._
import scalaz._

class FailureTests extends FreeSpec {

  val kernel = buildKernel()

  "compileFailure" in {
    checkFailure(kernel,
                 Vector(
                   ("java", {
                     case NonEmptyList(h, tl) => tl.isEmpty && h.msg.contains("package java is not a value")
                   }),
                   ("1 + vale", {
                     case NonEmptyList(h, tl) => tl.isEmpty && h.msg.contains("not found: value vale")
                   }),
                   ("1 + oogachaka; life; math.sqrt(true)", {
                     case x =>
                       (x.size == 3) && {
                         val checks: NonEmptyList[String => Boolean] =
                           NonEmptyList(_.contains("not found: value oogachaka"),
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

  "compilerCrash" in {
    check(kernel,
          Vector(
            ("val x = 1", {
              case Some(Success((_, x))) =>
                x match {
                  case _: Unit => true
                  case _ => false
                }
              case _ => false
            }),
            ("trait Bar { super[Object].hashCode}", {
              case Some(Failure(NonEmptyList(h, tl))) if tl.isEmpty =>
                h.msg.contains("java.lang.AssertionError: assertion failed")
              case _ => false
            }),
            ("1 + x", {
              case Some(Success((_, x))) =>
                x match {
                  case y: Int => y == 2
                  case _ => false
                }
              case _ => false
            })
          ))
  }

  // "ivyFail" in {
  //   check.session("""
  //       @ import $ivy.`com.lihaoyi::upickle:0.1.12312-DOESNT-EXIST`
  //       error: failed to resolve ivy dependencies
  //     """)
  // }

  "exceptionHandling" in {
    checkFailure(
      kernel,
      Vector(
        ("""throw new Exception("lol", new Exception("hoho"))""", {
          case NonEmptyList(h, tl) =>
            tl.isEmpty && (h.msg.contains("java.lang.Exception: lol")) && (h.msg.contains("java.lang.Exception: hoho"))
        })
      ))
  }

}
