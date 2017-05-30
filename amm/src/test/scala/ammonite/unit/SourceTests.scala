package ammonite.unit


import ammonite.TestUtils
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
      // The line number from first bytecode of earliest concrete method
      // may be inexact, but it should put you *somewhere* near what you
      // are looking for
      val nearby = loaded.fileContent.lines.slice(
        loaded.lineNum - 20,
        loaded.lineNum + 20
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
//        Not published for 2.10
//        check(
//          load(shapeless.::),
//          "hlists.scala",
//          "final case class ::"
//        )
        check(
          load(scopt.Read),
          "options.scala",
          "object Read"
        )
      }
      'stdLibScala{
        'direct - {
          // The Scala standard library classes for some reason don't get
          // properly included in the classpath in 2.10; it's unfortunate but
          // we'll just ignore it since the community has already moved on to
          // 2.11 and 2.12
          if (!TestUtils.scala2_10)check(
            load(Nil),
            "List.scala",
            "case object Nil extends List[Nothing]"
          )
        }
        'runtimeTyped - {
          if (!TestUtils.scala2_10){
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
      'fieldsAreTreatedAsObjects{
        // Can't use Java Std Lib methods because SBT screws up classloaders in test suite
        check(
          load(com.github.javaparser.JavaToken.INVALID),
          "JavaToken.java",
          "public class JavaToken"
        )
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
//        crashes compiler during GenBCode =/
//        check(
//          load(new javassist.ClassPool().find _),
//          "ClassPool.java",
//          "public URL find(String classname)"
//        )
      }
      'void{
        if (!TestUtils.scala2_10){
          check(
            load(Predef.println()),
            "Predef.scala",
            "def println() ="
          )
        }
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
        if (!TestUtils.scala2_10){

          val opt: Option[Int] = Option(1)
          check(
            load(opt.get),
            "Option.scala",
            "def get = "
          )
        }
      }
      'implementedBySuperclass{
        // The file has changed names since earlier versions...
        if (TestUtils.scala2_12){

          val list: List[Int] = List(1, 2, 3)
          check(
            load(list.toString),
            "SeqLike.scala",
            "override def toString = super[IterableLike].toString"
          )
        }
      }

    }
    'staticMethod{
      // Can't use Java Std Lib methods because SBT screws up classloaders in test suite
      check(
        load(com.github.javaparser.JavaParser.parseBlock _),
        "JavaParser.java",
        "public static BlockStmt parseBlock"
      )
    }
  }
}
