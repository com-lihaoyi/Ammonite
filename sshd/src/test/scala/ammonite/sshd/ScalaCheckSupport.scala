package ammonite.sshd

import org.scalacheck.Test.{PropException, Result, TestCallback}
import org.scalacheck.{Prop, Test}

trait ScalaCheckSupport {
  def check(prop: Prop): Unit = check(Test.Parameters.default.minSuccessfulTests)(prop)

  def check(minSuccessfulTests: Int)(prop: Prop): Unit = {
    val resultHolder = new ResultHolder
    prop.check {
      _.withTestCallback(resultHolder).withMinSuccessfulTests(minSuccessfulTests)
    }
    val result = resultHolder.result
    result.status match {
      case PropException(_, e, _) ⇒ throw e
      case _ ⇒ assert(result.passed)
    }
  }

  implicit def utestCheckToProp: Unit ⇒ Prop = { (_) ⇒ Prop.apply(true) }

  class ResultHolder extends TestCallback {
    var result: Result = _

    override def onTestResult(name: String, result: Result): Unit = {
      this.result = result
    }

    override def toString = s"ResultHolder($result)"
  }
}
