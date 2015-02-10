package ammonite.repl

import ammonite.repl.frontend._
import utest._


class Checker {
  lazy val replApi: ReplAPI = new DefaultReplAPI(
    Nil,
    interp.loadJar,
    (groupId, artifactId, version) => {
      interp.loadJar(IvyThing.resolveArtifact(groupId, artifactId, version))
    },
    () => interp.init(),
    ColorSet.BlackWhite,
    ammonite.pprint.Config.Defaults.PPrintConfig
  )

  val interp: Interpreter = new Interpreter(replApi)

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
