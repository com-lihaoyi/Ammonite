package ammonite.repl

import java.io.File
import java.net.URLClassLoader

import ammonite.IvyThing
import ammonite.repl.eval
import ammonite.repl.eval.{Classpath, Evaluator, Compiler, Preprocessor}
import ammonite.repl.frontend._
import utest._

import scala.collection.mutable
import scala.reflect.runtime.universe._
import scala.reflect.io.VirtualDirectory


class Checker {
  val interp: Interpreter = new Interpreter(() => initReplBridge())
  def initReplBridge() = {
    interp.compiler.importsFor("", interp.eval.replBridgeCode)
    val cls = interp.eval.evalClass(interp.eval.replBridgeCode, "ReplBridge")
    ReplAPI.initReplBridge(
      cls.asInstanceOf[Result.Success[Class[ReplAPIHolder]]].s,
      replAPI
    )
  }


  lazy val replAPI: ReplAPI = new DefaultReplAPI(
    Nil,
    interp.loadJar,
    (groupId, artifactId, version) => {
      interp.loadJar(IvyThing.resolveArtifact(groupId, artifactId, version))
    },
    () => {
      interp.initCompiler()
      initReplBridge()
    },
    ColorSet.BlackWhite,
    ammonite.pprint.Config.Defaults.PPrintConfig
  )

  initReplBridge()

  def apply(input: String,
            expected: String = null) = {
    print(".")
    val processed = interp.eval.processLine(input)
    val printed = processed.map(_.msg)

    if (expected != null)
      assert(printed == Result.Success(expected.trim))

    interp.eval.update(processed)
  }
  def fail(input: String,
           failureCheck: String => Boolean = _ => true) = {
    print(".")
    val processed = interp.eval.processLine(input)
    val printed = processed.map(_.msg)
    printed match{
      case Result.Success(v) => assert({v; false})
      case Result.Failure(s) => assert(failureCheck(s))
    }

    interp.eval.update(processed)
  }
}
