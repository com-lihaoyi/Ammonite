package ammonite.kernel

import ammonite.runtime.Storage
import ammonite.util._
import scalaz.{Name => _, _}
import kernel._
import org.scalatest.Assertions._

object KernelTests {

  private val defaultPredef = Seq(
    Name("defaultPredef") -> ammonite.main.Defaults.predefString
  )

  def buildKernel(predefs: Seq[(Name, String)] = defaultPredef) = new ReplKernel(new Storage.InMemory, predefs)

  val checkUnit: Any => Boolean = {
    case _ : Unit => true
    case _ => false
  }

  def checkInt(i: Int): Any => Boolean = {
    case x : Int => x == i
    case _ => false
  }

  def checkLong(l: Long): Any => Boolean = {
    case x: Long => x == l
    case _ => false
  }

  def checkString(s: String): Any => Boolean = {
    case x: String => x == s
    case _ => false
  }

  def checkChar(c: Char): Any => Boolean = {
    case x: Char => x == c
    case _ => false
  }

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

}
