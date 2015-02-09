package ammonite.repl.eval

import java.lang.reflect.InvocationTargetException

import acyclic.file
import ammonite.repl.frontend.{ReplAPI, ReplAPIHolder}
import ammonite.repl.{Evaluated, Result, Catching}
import scala.reflect.io.VirtualDirectory
import scala.reflect.runtime.universe._
import scala.collection.mutable
import scala.util.Try

/**
 * Takes source code and, with the help of a compiler and preprocessor,
 * evaluates it and returns a `Result[(output: String, imports: String)]`
 * where `output` is what gets printed and `imports` are any imports that
 * need to get prepended to subsequent commands.
 */
trait Evaluator {
  def initReplBridge(replAPI: ReplAPI): Unit
  def previousImportBlock: String
  def processLine(line: String): Result[Evaluated]
  def update(res: Result[Evaluated]): Unit
  def setJars(jarDeps: Seq[java.io.File],
              dirDeps: Seq[java.io.File],
              dynamicClasspath: VirtualDirectory): Unit
  def complete(index: Int, allCode: String): (Int, Seq[String])
  def askShutdown(): Unit
}

object ScalaEvaluator {
  def namesFor(t: scala.reflect.runtime.universe.Type): Set[String] = {
    val yours = t.members.map(_.name.toString).toSet
    val default = typeOf[Object].members.map(_.name.toString)
    yours -- default
  }

  val defaultInitImports = Seq(
    "PPrintConfig" -> "import ammonite.pprint.Config.Colors.PPrintConfig"
  ) ++ {
    namesFor(typeOf[ReplAPI]).map(n => n -> s"import ReplBridge.shell.$n")
  }
}

class ScalaEvaluator(currentClassloader: ClassLoader,
                extraClassLoaders: => Seq[ClassLoader],
                initImports: Seq[(String, String)] = ScalaEvaluator.defaultInitImports) extends Evaluator {

  private var compiler: Compiler = _
  private var preprocess: Preprocessor = _
  def setJars(jarDeps: Seq[java.io.File],
                      dirDeps: Seq[java.io.File],
                      dynamicClasspath: VirtualDirectory) = {
    compiler = new ScalaCompiler(
      jarDeps,
      dirDeps,
      dynamicClasspath,
      println
    )
    preprocess = new ScalaPreprocessor(compiler.parse)
  }

  def askShutdown(): Unit = {
    if (compiler != null)
      compiler.askShutdown()
  }

  def complete(index: Int, allCode: String): (Int, Seq[String]) = compiler.complete(index, allCode)

  def initReplBridge(replAPI: ReplAPI): Unit = {
    compiler.importsFor("", replBridgeCode)
    val cls = evalClass(replBridgeCode, "ReplBridge")
    ReplAPI.initReplBridge(
      cls.asInstanceOf[Result.Success[Class[ReplAPIHolder]]].s,
      replAPI
    )
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
  val previousImports = mutable.Map(initImports: _*)


  val replBridgeCode =
    "object ReplBridge extends ammonite.repl.frontend.ReplAPIHolder{}"
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
    override def loadClass(name: String): Class[_] = {
      if (newFileDict.contains(name)) {
        defineClass(name, newFileDict(name), 0, newFileDict(name).length)
      }else{
        try{
          super.loadClass(name)
        }catch{ case e: ClassNotFoundException =>
          val classes = for(cl <- extraClassLoaders.iterator) yield {
            try Some(cl.loadClass(name))
            catch{ case e: ClassNotFoundException => None}
          }
          classes.collectFirst{ case Some(cls) => cls}
                 .headOption
                 .getOrElse{ throw new ClassNotFoundException(name) }
        }
      }
    }
  }

  def evalClass(code: String, wrapperName: String): Result[Class[_]] = for{
    compiled <- Result(Try(
      compiler.compile(code.getBytes)
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
        // Clear the interrupted status
        Thread.interrupted()
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

  def previousImportBlock = {
    previousImports.toSeq
      .sortBy(_._1)
      .map(_._2)
      .mkString("\n")
  }
  def processLine(line: String) = for {
    Preprocessor.Output(code, printer) <- preprocess(line, currentLine)

    wrapperName = "cmd" + currentLine
    wrapped = s"""
      object Foo$wrapperName{   }
      object $wrapperName{
        $code
        def $$main() = {$printer}
      }
    """

    wrappedWithImports = previousImportBlock + "\n\n" + wrapped
    evaled <- evalMain(wrappedWithImports, wrapperName)
    newImports = compiler.importsFor(wrapperName, wrappedWithImports)

  } yield Evaluated(evaled + "", wrapperName , newImports)

  def update(res: Result[Evaluated]) = res match {
    case Result.Success(ev) =>
      for{
        (name, proposedImport) <- ev.imports
        if name != "package"
      }{
        previousImports(name) = s"$proposedImport.`$name`"
      }
      currentLine += 1
    case Result.Failure(msg) =>
      currentLine += 1
    case _ =>
  }
}
