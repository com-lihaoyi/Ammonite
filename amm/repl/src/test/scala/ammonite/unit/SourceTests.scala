package ammonite.unit


import ammonite.repl.tools.Location
import utest._
import ammonite.repl.tools.source.load
import ammonite.util.Util

import java.io.InputStream
object SourceTests extends TestSuite{
  val tests = Tests{

    def check(loaded: Location, expectedFileName: String, expected: String, slop: Int = 10) = {


      val loadedFileName = loaded.fileName
      assert(loadedFileName == expectedFileName)
      // The line number from first bytecode of earliest concrete method
      // may be inexact, but it should put you *somewhere* near what you
      // are looking for
      val nearby = Predef.augmentString(loaded.fileContent).lines.slice(
        loaded.lineNum - slop,
        loaded.lineNum + slop
      ).mkString(Util.newLine)
      assert(nearby.contains(expected))
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
          check(
            load(Nil),
            "List.scala",
            "case object Nil extends List[Nothing]"
          )
        }
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

        check(
          load(new javassist.ClassPool().find _),
          "ClassPool.java",
          "public URL find(String classname)"
        )
      }
      'void{
        check(
          load(Predef.println()),
          "Predef.scala",
          "def println()"
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
          "def get"
        )
      }
    }
  }
}
