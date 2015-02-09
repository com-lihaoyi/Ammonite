package ammonite.repl

import java.io.File

import ammonite.repl.eval
import ammonite.repl.eval.{Classpath, Evaluator, Compiler, Preprocessor}
import ammonite.repl.frontend.{FullReplAPI, ReplAPI, ReplAPIHolder}
import utest._

import scala.collection.mutable
import scala.reflect.runtime.universe._
import scala.reflect.io.VirtualDirectory


class Checker {
  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  val compiler = new Compiler(Classpath.jarDeps, Classpath.dirDeps, dynamicClasspath, println)
  val preprocess = new Preprocessor(compiler.parse)
  val eval = new Evaluator(
    Thread.currentThread().getContextClassLoader,
    preprocess.apply,
    compiler.compile,
    compiler.importsFor
  ){
    override val previousImports = mutable.Map(
      "PPrintConfig" -> "import ammonite.pprint.Config.Defaults.PPrintConfig"
    )
  }
  compiler.importsFor("", eval.replBridgeCode)
  val cls = eval.evalClass(eval.replBridgeCode, "ReplBridge")

  ReplAPI.initReplBridge(
    cls.asInstanceOf[Result.Success[Class[ReplAPIHolder]]].s,
    new FullReplAPI {
      def help: String = "Hello!"
      def history: Seq[String] = Seq("1")
      def shellPPrint[T: WeakTypeTag](value: => T, ident: String) = {
        ident + ": " + weakTypeOf[T].toString
      }
      def shellPrintDef(definitionLabel: String, ident: String) = {
        s"defined $definitionLabel $ident"
      }
      var shellPrompt = "scala>"

      def load(jar: File) = ()
      def newCompiler() = ()
    }
  )
  def apply(input: String, expected: String = null) = {
    print(".")
    val processed = eval.processLine(input)
    val printed = processed.map(_.msg)
    if (expected != null)
      assert(printed == Result.Success(expected.trim))
    eval.update(processed)
  }

}
