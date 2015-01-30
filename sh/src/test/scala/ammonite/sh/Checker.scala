package ammonite.sh

import ammonite.sh.eval
import ammonite.sh.eval.{Evaluator, Compiler, Preprocessor}
import utest._

import scala.collection.mutable
import scala.reflect.io.VirtualDirectory

/**
 * Created by haoyi on 1/20/15.
 */
class Checker {
  val preprocess = new Preprocessor{
    override def pprintSignature(ident: String) = s"""
          "$ident" +  ": " +
          ammonite.sh.Shell.typeString($ident)"""

  }
  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  val compiler = new Compiler(dynamicClasspath)
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
  def apply(input: String, expected: String = null) = {
    val processed = eval.processLine(input)
    val printed = processed.map(_.msg)
    if (expected != null)
      assert(printed == Result.Success(expected.trim))
    eval.update(processed)
  }
}
