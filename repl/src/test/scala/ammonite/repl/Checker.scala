package ammonite.repl

import java.io.File

import ammonite.repl.eval._
import ammonite.repl.frontend.FullReplAPI
import utest._

import scala.reflect.runtime.universe._
import scala.reflect.io.VirtualDirectory


class Checker {
  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  val eval: Evaluator = new ScalaEvaluator(
    Thread.currentThread().getContextClassLoader,
    Nil,
    Seq("PPrintConfig" -> "import ammonite.pprint.Config.Defaults.PPrintConfig")
  )

  eval.setJars(Classpath.jarDeps, Classpath.dirDeps, dynamicClasspath)

  eval.initReplBridge(new FullReplAPI {
    def help: String = "Hello!"
    def history: Seq[String] = Seq("1")
    def shellPPrint[T: WeakTypeTag](value: => T, ident: String) = {
      ident + ": " + weakTypeOf[T].toString
    }
    def shellPrintDef(definitionLabel: String, ident: String) = {
      s"defined $definitionLabel $ident"
    }
    var shellPrompt = "scala>"

    // Not needed for tests
    def load(jar: File) = ()
    def newCompiler() = ()
    def loadIvy(groupId: String, artifactId: String, version: String) = ()
  })

  def apply(input: String, expected: String = null) = {
    print(".")
    val processed = eval.processLine(input)
    val printed = processed.map(_.msg)
    if (expected != null)
      assert(printed == Result.Success(expected.trim))
    eval.update(processed)
  }

}
