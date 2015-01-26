package ammonite.sh.eval

import java.lang.reflect.InvocationTargetException

import acyclic.file
import ammonite.sh.{Result, Catching}

import scala.collection.mutable
import scala.util.Try

/**
 * Takes source code and, with the help of a compiler and preprocessor,
 * evaluates it and returns a `Result[(output: String, imports: String)]`
 * where `output` is what gets printed and `imports` are any imports that
 * need to get prepended to subsequent commands.
 */
class Evaluator(currentClassloader: ClassLoader,
                preprocess: (String, Int) => Option[(String, String, String)],
                compile: (Array[Byte], String => Unit) => Compiler.Output) {

  /**
   * Files which have been compiled, stored so that our special 
   * classloader can get at them.
   */
  val newFileDict = mutable.Map.empty[String, Array[Byte]]
  /**
   * Imports which are required by earlier commands to the REPL. Imports
   * have a specified key, so that later imports of the same name (e.g.
   * defining a variable twice) can kick the earlier import out of the
   * map. Otherwise if you import the same name twice you get compile
   * errors instead of the desired shadowing.
   */
  val previousImports = mutable.Map(
    "PPrintConfig" -> "import ammonite.pprint.Config.Defaults.PPrintConfig"
  )
  /**
   * The current line number of the REPL, used to make sure every snippet
   * evaluated can have a distinct name that doesn't collide.
   */
  var currentLine = 0
  /**
   * Performs the conversion of our pre-compiled `Array[Byte]`s into
   * actual classes with methods we can execute.
   */
  val evalClassloader = new ClassLoader(currentClassloader) {
    override def loadClass(name: String) = {
      if (!newFileDict.contains(name))super.loadClass(name)
      else defineClass(name, newFileDict(name), 0, newFileDict(name).length)
    }
  }

  def processLine(line: String) = for {
    (importKey, imports, wrapped) <- Result(
      preprocess(line, currentLine),
      "Don't recognize that input =/"
    )

    wrappedWithImports = previousImports.toSeq.sortBy(_._1).map(_._2).mkString("\n") + "\n\n" + wrapped

    compiled <- Result(Try(
      compile(wrappedWithImports.getBytes, println)
    ), e => {println("!!!! " + e.printStackTrace()); e.toString})

    classFiles <- Result(compiled, "Compilation Failed")

    (cls, method) <- Result(Try {
      for ((name, bytes) <- classFiles) {
        newFileDict(name) = bytes
      }

      val cls = Class.forName("$res" + currentLine, true, evalClassloader)
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
  } yield (method.invoke(null) + "", importKey, imports)

  def update(res: Result[(String, String, String)]) = res match {
    case Result.Success((msg, importKey, imports)) =>
      previousImports(importKey) = imports
      currentLine += 1
    case Result.Failure(msg) =>
      currentLine += 1
  }
}
