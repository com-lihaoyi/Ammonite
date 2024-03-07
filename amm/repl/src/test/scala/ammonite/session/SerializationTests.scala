package ammonite.session

import ammonite.TestRepl
import ammonite.TestUtils
import ammonite.compiler.CodeClassWrapper
import utest._

object SerializationTests extends TestSuite {
  val tests = if (TestUtils.scala2) scala2Tests else scala3Tests
  def scala2Tests = Tests {
    println("SerializationTests")
    val check = new TestRepl {
      override def codeWrapper = CodeClassWrapper
    }

    test("closure") {
      // User values from the REPL shouldn't be recomputed upon
      // deserialization. The test below checks that the value `a`, whose
      // computation is side-effecting, is indeed serialized along `closure`,
      // rather than re-computed.

      // About the issue of values in singletons not being serialized (and
      // being recomputed if necessary), see the discussion in
      // https://groups.google.com/forum/#!topic/scala-internals/h27CFLoJXjE.

      // This is similar to what happens when mapping a function on an RDD
      // or using a UDF in Spark: a closure gets serialized this way, and is sent
      // to a bunch of "executors" that each deserialize it.

      // In the test, `a` acts as the result of the heavy computation. We do not want
      // the calculation of `a` to re-run when deserializing `closure`.

      // After creating the closure, the test serializes it, then deserializes it
      // via a classloader that can load the exact same bytecode as the classloader of
      // the current session. We call the deserialized closure by reflection, and check
      // that the heavy computation was not run again.

      // -Ydelambdafy=inline seems to be required in 2.12 for serializing lambdas

      check.session(
        s"""
          @ interp.configureCompiler(_.settings.Ydelambdafy.value = "inline")

          @ object costlySideEffect {
          @   import java.nio.file.{Files, Paths}
          @   private val path = Paths.get("a")
          @   def apply()  = Files.write(path, Array[Byte]())
          @   def clear()  = Files.deleteIfExists(path)
          @   def exists() = assert(Files.exists(path), "Side-effect didn't run")
          @   def absent(msg: String = "") =
          @     assert(!Files.exists(path), s"Side-effect did run $$msg")
          @ }

          @ class D // not serializable

          @ val d =
          @   // serialization should succeed in spite of this
          @   // non-serializable variable being around
          @   new D

          @ val a = {
          @   costlySideEffect()
          @   Option(2)
          @ }

          @ val closure: Int => Option[Int] = { n =>
          @   a.map(_ + n)
          @ }

          @ {{
          @   costlySideEffect.exists() // "a" side-effect has run
          @   costlySideEffect.clear()
          @   costlySideEffect.absent("after cleaning")
          @ }}

          @ lazy val closureClone = ammonite.SerializationUtil.deserialize(
          @   ammonite.SerializationUtil.serialize(closure),
          @   Thread.currentThread
          @     .getContextClassLoader
          @     .asInstanceOf[{ def cloneClassLoader(parent: ClassLoader): ClassLoader }]
          @     .cloneClassLoader(null)
          @ )

          @ {{
          @   costlySideEffect.absent("before deserialization")
          @   closureClone
          @     .getClass
          @     .getMethod("apply", classOf[Int])
          @     .invoke(closureClone, 2: java.lang.Integer)
          @   // calling apply should not run the side-effect again, the
          @   // value of 'a' must come from deserialization
          @   costlySideEffect.absent("after deserialization")
          @ }}

        """
      )
    }
  }
  def scala3Tests = Tests {
    // delambdafy not supported by Scala 3?
    test("disabled") { "disabled for Scala 3" }
  }
}
