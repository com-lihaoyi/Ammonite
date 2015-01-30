package ammonite.sh.eval

import java.lang.reflect.InvocationTargetException

import acyclic.file
import ammonite.sh.{ShellAPIs, Evaluated, Result, Catching}
import scala.reflect.runtime.universe._
import scala.collection.mutable
import scala.util.Try

/**
 * Takes source code and, with the help of a compiler and preprocessor,
 * evaluates it and returns a `Result[(output: String, imports: String)]`
 * where `output` is what gets printed and `imports` are any imports that
 * need to get prepended to subsequent commands.
 */
class Evaluator(currentClassloader: ClassLoader,
                preprocess: (String, Int) => Option[Preprocessor.Output],
                compile: (Array[Byte], String => Unit) => Compiler.Output,
                importsFor: (String, String) => Seq[(String, String)]) {

  def namesFor(t: scala.reflect.runtime.universe.Type): Set[String] = {
    val yours = t.members.map(_.name.toString).toSet
    val default = typeOf[Object].members.map(_.name.toString)
    yours -- default
  }

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
    "PPrintConfig" -> "import ammonite.pprint.Config.Colors.PPrintConfig"
  ) ++ namesFor(typeOf[ShellAPIs]).map(n => n -> s"import ShellBridge.shell.$n")


  val shellBridgeCode =
    "object ShellBridge extends ammonite.sh.ShellAPIHolder{}"
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

  def evalClass(code: String, wrapperName: String): Result[Class[_]] = for{
    compiled <- Result(Try(
      compile(code.getBytes, println)
    ), e => {println("!!!! " + e.printStackTrace()); e.toString})

    classFiles <- Result[Traversable[(String, Array[Byte])]](compiled, "Compilation Failed")

    cls <- Result[Class[_]](Try {
      for ((name, bytes) <- classFiles) {
        newFileDict(name) = bytes
      }

      Class.forName(wrapperName , true, evalClassloader)
    }, e => "Failed to load compiled class " + e)
  } yield cls

  def evalMain(code: String, wrapperName: String) = for{

    cls <- evalClass(code, wrapperName)

    method = cls.getDeclaredMethod("$main")

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
  } yield method.invoke(null)

  var evalId = 0

  def evalExpr(code: String) = {
    val wrapperId = "$eval" + evalId
    evalId += 1
    evalMain(s"""
      object $wrapperId{
        def $$main() = {
          $code
        }
      }""",
      wrapperId
    )
  }

  def processLine(line: String) = for {
    Preprocessor.Output(code, printer) <- Result(
      preprocess(line, currentLine),
      "Don't recognize that input =/"
    )

    wrapperName = "$res" + currentLine
    wrapped = s"""
      object Foo$wrapperName{   }
      object $wrapperName{
        $code
        def $$main() = {$printer}
      }
    """
    wrappedWithImports =
      previousImports.toSeq
                     .sortBy(_._1)
                     .map(_._2)
                     .mkString("\n") + "\n\n" + wrapped

    newImports = importsFor(wrapperName, wrappedWithImports)
    evaled <- evalMain(wrappedWithImports, wrapperName)

  } yield Evaluated(evaled + "", wrapperName , newImports)

  def update(res: Result[Evaluated]) = res match {
    case Result.Success(ev) =>
      for((name, proposedImport) <- ev.imports){
        previousImports(name) = proposedImport + "." + name
      }
      currentLine += 1
    case Result.Failure(msg) =>
      currentLine += 1
  }
}
