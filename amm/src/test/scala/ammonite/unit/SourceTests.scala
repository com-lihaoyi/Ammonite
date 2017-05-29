package ammonite.unit

import javassist.bytecode.ClassFile

import ammonite.ops._
import utest._
import ammonite.repl.tools.source.{loadObjectInfo, loadObjectMemberInfo}

import scala.tools.nsc.interpreter.InputStream
object SourceTests extends TestSuite{
  override def utestTruncateLength = 500000
  def check(lhs: String, rhs: String) = {
    assert(lhs == rhs)
  }
  val pprinter = pprint.PPrinter.BlackWhite.copy(
    additionalHandlers = ammonite.repl.PPrints.replPPrintHandlers
  )
  val tests = TestSuite{
    def check(loaded: Either[String, (String, String, Int)], expectedFileName: String, expected: String*) = {
      val Right((fileName, sourceCode, lineNum)) = loaded
      assert(fileName == expectedFileName)
      val nearby = sourceCode.lines.slice(lineNum - 10, lineNum + 10).mkString("\n")
      for(snippet <- expected){
        assert(nearby.contains(snippet))
      }
    }
    'objectInfo{

      'thirdPartyJava{
        check(
          loadObjectInfo(new javassist.ClassPool()),
          "ClassPool.java",
          "public class ClassPool"
        )
      }
      'thirdPartyScala{
        check(
          loadObjectInfo(shapeless.::),
          "hlists.scala",
          "final case class ::"
        )
        check(
          loadObjectInfo(scopt.Read),
          "options.scala",
          "object Read"
        )
      }
      'stdLibScala{
        'direct - check(
          loadObjectInfo(Nil),
          "List.scala",
          "case object Nil extends List[Nothing]"
        )
        'runtimeTyped - {
          check(
            loadObjectInfo(Seq()),
            "List.scala",
            "case object Nil extends List[Nothing]"
          )
          check(
            loadObjectInfo(Seq(1)),
            "List.scala",
            "final case class ::"
          )
        }
      }
    }
    'objectMemberInfo{
      'thirdPartyJava{
        check(
          loadObjectMemberInfo(new javassist.ClassPool(), "get", Seq(classOf[String])),
          "ClassPool.java",
          "public CtClass get(String classname)"
        )
      }
      'overloaded{
        val pool = new javassist.ClassPool()
        check(
          loadObjectMemberInfo(pool, "makeClass", Seq(classOf[InputStream])),
          "ClassPool.java",
          "public CtClass makeClass(InputStream classfile)"
        )
        check(
          loadObjectMemberInfo(pool, "makeClass", Seq(classOf[String])),
          "ClassPool.java",
          "public CtClass makeClass(String classname)"
        )
        check(
          loadObjectMemberInfo(pool, "makeClass", Seq(classOf[ClassFile], classOf[Boolean])),
          "ClassPool.java",
          "public CtClass makeClass(ClassFile classfile, boolean ifNotFrozen)"
        )
      }
      'implementedBySubclass{
        check(
          loadObjectMemberInfo(Option(1), "get", Nil),
          "Option.scala",
          "def get = value"
        )
      }
      'implementedBySuperclass{
        check(
          loadObjectMemberInfo(List(1, 2, 3), "toString", Nil),
          "Seq.scala",
          "def get = value"
        )
      }

    }

  }
}
