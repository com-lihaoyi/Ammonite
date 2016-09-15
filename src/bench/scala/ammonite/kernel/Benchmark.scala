package ammonite.kernel

import BenchmarkConstants._
import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import reflect.runtime._
import tools.reflect.ToolBox

object AmmoniteBenchmarks extends Bench.OfflineReport {


  val gen = Gen.single("TestString")(staticString)

  performance of "Compilation" in {
    measure method "kernel" in {
      using(gen) in { s =>
        kernelInstance.process(s)
      }
    }
    measure method "reflect" in {
      using(gen) in { s =>
        reflectCompile(s)
      }
    }
  }

}

object BenchmarkConstants {

  val kernelInstance = ReplKernel()
  val tb = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()

  def reflectCompile(str: String): Any = {
    tb.eval(tb.parse(str))
  }

  val staticString =
    """
    def foo(s1: Seq[Int], s2: Seq[Int]): String = s1.mkString + s2.mkString
    foo(Nil, Nil)
    """

}
