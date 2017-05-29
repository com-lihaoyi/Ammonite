package ammonite.unit


import ammonite.ops._
import ammonite.repl.tools.Location
import utest._
import ammonite.repl.tools.source.load

import scala.tools.nsc.interpreter.InputStream
object SourceTests extends TestSuite{
  override def utestTruncateLength = 500000
  val tests = TestSuite{
    def check(loaded: Location, expectedFileName: String, expected: String*) = {

      assert(loaded.fileName == expectedFileName)
      val nearby = loaded.fileContent.lines.slice(
        loaded.lineNum - 10,
        loaded.lineNum + 10
      ).mkString("\n")
      for(snippet <- expected){
        assert(nearby.contains(snippet))
      }
    }

    'objectInfo{
      'thirdPartyJava{
        check(
          load(new javassist.ClassPool()),
          "ClassPool.java",
          "public class ClassPool"
        )
      }
      'thirdPartyScala{
        check(
          load(shapeless.::),
          "hlists.scala",
          "final case class ::"
        )
        check(
          load(scopt.Read),
          "options.scala",
          "object Read"
        )
      }
      'stdLibScala{
        'direct - check(
          load(Nil),
          "List.scala",
          "case object Nil extends List[Nothing]"
        )
        'runtimeTyped - {
          val empty: Seq[Int] = Seq()
          val nonEmpty: Seq[Int] = Seq(1)
          check(
            load(empty),
            "List.scala",
            "case object Nil extends List[Nothing]"
          )
          check(
            load(nonEmpty),
            "List.scala",
            "final case class ::"
          )
        }
      }
    }
    'objectMemberInfo{
      'thirdPartyJava{
        val pool = new javassist.ClassPool()
        check(
          load(pool.find _),
          "ClassPool.java",
          "public URL find(String classname)"
        )
      }

      'overloaded{
        val pool = new javassist.ClassPool()
        check(
          load(pool.makeClass(_: InputStream)),
          "ClassPool.java",
          "public CtClass makeClass(InputStream classfile)"
        )
        check(
          load(pool.makeClass(_: String)),
          "ClassPool.java",
          "public CtClass makeClass(String classname)"
        )
        check(
          load(pool.makeClass(_: javassist.bytecode.ClassFile, _: Boolean)),
          "ClassPool.java",
          "public CtClass makeClass(ClassFile classfile, boolean ifNotFrozen)"
        )
      }
      'implementedBySubclass{
        val opt: Option[Int] = Option(1)
        check(
          load(opt.get),
          "Option.scala",
          "def get = value"
        )
      }
      'implementedBySuperclass{
        val list: List[Int] = List(1, 2, 3)
        check(
          load(list.toString),
          "SeqLike.scala",
          "override def toString = super[IterableLike].toString"
        )
      }

    }

  }
}
