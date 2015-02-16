package ammonite.repl

import ammonite.repl.frontend._
import ammonite.repl.interp.Interpreter
import utest._


class Checker {
  lazy val replApi: ReplAPI = new DefaultReplAPI(
    Nil,
    interp.loadJar,
    _.foreach{line =>
      handleResult(interp.eval.processLine(line))
    },
    (groupId, artifactId, version) => {
      interp.loadJar(IvyThing.resolveArtifact(groupId, artifactId, version))
    },
    () => (),
    () => interp.init(),
    ColorSet.BlackWhite,
    ammonite.pprint.Config.Defaults.PPrintConfig
  )

  val interp: Interpreter = new Interpreter(replApi)

  def handleResult(res: Result[Evaluated]) = {
    interp.eval.update(res.asInstanceOf[Result.Success[Evaluated]].s.imports)
  }
  def apply(input: String,
            expected: String = null) = {
    print(".")
    val processed = interp.eval.processLine(input)
    val printed = processed.map(_.msg.mkString)

    if (expected != null)
      assert(printed == Result.Success(expected.trim))

    handleResult(processed)
  }
  def fail(input: String,
           failureCheck: String => Boolean = _ => true) = {
    print(".")
    val processed = interp.eval.processLine(input)
    val printed = processed.map(_.msg.mkString)

    printed match{
      case Result.Success(v) => assert({v; false})
      case Result.Failure(s) => assert(failureCheck(s))
    }

  }
}
