package ammonite.sh2.eval

import java.lang.reflect.InvocationTargetException

import acyclic.file
import ammonite.sh2.{Result, Catching}

import scala.collection.mutable
import scala.tools.nsc.io._
import scala.util.Try


class Evaluator(currentClassloader: ClassLoader,
                preprocess: (String, Int) => Option[(String, String, String)],
                compile: (Array[Byte], String => Unit) => Option[Traversable[(String, Array[Byte])]]) {

  val newFileDict = mutable.Map.empty[String, Array[Byte]]
  val previousImports = mutable.Buffer.empty[String]
  var currentLine = 0

  val execClassloader = new ClassLoader(currentClassloader) {

    override def loadClass(name: String) = {
      if (!newFileDict.contains(name))super.loadClass(name)
      else defineClass(name, newFileDict(name), 0, newFileDict(name).length)
    }
  }

  def processLine(line: String) = for {
    (imports, wrapped, name) <- Result(
      preprocess(line, currentLine),
      "Don't recognize that input =/"
    )

    wrappedWithImports = previousImports.mkString("\n") + wrapped

    compiled <- Result(Try(
      compile(wrappedWithImports.getBytes, println)
    ), e => e.toString)

    classFiles <- Result(compiled, "Compilation Failed")

    (cls, method) <- Result(Try {
      for ((name, bytes) <- classFiles) {
        newFileDict(name) = bytes
      }

      val cls = Class.forName("$" + name, true, execClassloader)
      (cls, cls.getDeclaredMethod("$main"))
    }, e => "Failed to load compiled class " + e)

    _ <- Catching{
      case ex: InvocationTargetException
        if ex.getCause.isInstanceOf[ExceptionInInitializerError]  =>
        val userEx = ex.getCause .getCause
        val trace =
          userEx
            .getStackTrace
            .takeWhile(x => !(x.getClassName == cls.getName && x.getMethodName == "$main"))
            .mkString("\n")

        userEx.toString + "\n" + trace
      case ex: InvocationTargetException
        if ex.getCause.isInstanceOf[ThreadDeath]  =>
        "\nInterrupted!"
    }
  } yield (method.invoke(null) + "", imports)

  def update(res: Result[(String, String)]) = res match {
    case Result.Success((msg, imports)) =>
      previousImports.append(imports)
      currentLine += 1
    case Result.Failure(msg) =>
      currentLine += 1
  }
}
