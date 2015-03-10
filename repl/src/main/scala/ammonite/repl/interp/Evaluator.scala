package ammonite.repl.interp

import java.lang.reflect.InvocationTargetException
import java.net.URL

import acyclic.file
import ammonite.repl.frontend.{ReplExit, ReplAPI}
import ammonite.repl._
import java.net.URLClassLoader
import scala.reflect.runtime.universe._
import scala.collection.mutable
import scala.util.Try

/**
 * Takes source code and, with the help of a compiler and preprocessor,
 * evaluates it and returns a `Result[(output: String, imports: String)]`
 * where `output` is what gets printed and `imports` are any imports that
 * need to get prepended to subsequent commands.
 */
trait Evaluator{
  def evalClass(code: String, wrapperName: String): Res[(Class[_], Seq[ImportData])]
  def getCurrentLine: Int
  def update(newImports: Seq[ImportData]): Unit

  /**
   * Takes the preprocessed `code` and `printCode` and compiles/evals/runs/etc.
   * it to provide a result. Takes `printer` as a callback, instead of returning
   * the `Iterator` as part of the output, because printing can cause side effects
   * (e.g. for Streams which are lazily printed) and can fail with an exception!
   * passing in the callback ensures the printing is still done lazily, but within
   * the exception-handling block of the `Evaluator`
   */
  def processLine(code: String, printCode: String, printer: Iterator[String] => Unit): Res[Evaluated]

  def previousImportBlock: String
  def addJar(url: URL): Unit
  def newClassloader(): Unit
  def evalClassloader: ClassLoader
}

object Evaluator{
  def apply(currentClassloader: ClassLoader,
            preprocess: (String, Int) => Res[Preprocessor.Output],
            compile: => (Array[Byte], String => Unit) => Compiler.Output,
            stdout: String => Unit): Evaluator = new Evaluator{

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
    lazy val previousImports = mutable.Map(
      namesFor(typeOf[ReplAPI]).map(n => n -> ImportData(n, None, "", "ReplBridge.shell")).toSeq ++
      namesFor(typeOf[ammonite.repl.IvyConstructor]).map(n => n -> ImportData(n, None, "", "ammonite.repl.IvyConstructor")).toSeq
      :_*
    )


    /**
     * The current line number of the REPL, used to make sure every snippet
     * evaluated can have a distinct name that doesn't collide.
     */
    var currentLine = 0

    /**
     * Weird indirection only necessary because of
     * https://issues.scala-lang.org/browse/SI-7085
     */
    def getCurrentLine = currentLine

    /**
     * Performs the conversion of our pre-compiled `Array[Byte]`s into
     * actual classes with methods we can execute.
     *
     * Structured such that when a class is desired:
     *
     * - First we try to load it with the REPL's "root" classloader
     * - If we can't find it there, we slowly start making our way
     *   up from the current classloader back up to the root
     *
     * This has the property that if you import something, later imports
     * take precedence, although you don't end up with weird bugs
     * re-defining the core (pre-REPL) classes. I'm still not sure
     * where those come from.
     */
    var evalClassloader =
      new URLClassLoader(Array(), currentClassloader) {
        // Public access to addURL - a visibility-changing override fails here
        def add(url: URL) = addURL(url)
      }


    def newClassloader() = {
      evalClassloader = new URLClassLoader(Array(), evalClassloader){
        def add(url: URL) = addURL(url)
        override def loadClass(name: String): Class[_] = {
          if(newFileDict.contains(name)) {
            val bytes = newFileDict(name)
            defineClass(name, bytes, 0, bytes.length)
          }
          else try currentClassloader.loadClass(name)
          catch{ case e: ClassNotFoundException =>
            try this.findClass(name)
            catch{ case e: ClassNotFoundException =>
              super.loadClass(name)
            }
          }
        }
      }
    }
    newClassloader()

    def addJar(url: URL) = evalClassloader.add(url)

    def evalClass(code: String, wrapperName: String) = for{

      (output, compiled) <- Res.Success{
        val output = mutable.Buffer.empty[String]
        val c = compile(code.getBytes, output.append(_))
        (output, c)
      }

      (classFiles, importData) <- Res[(Traversable[(String, Array[Byte])], Seq[ImportData])](
        compiled, "Compilation Failed\n" + output.mkString("\n")
      )

      cls <- Res[Class[_]](Try {
        for ((name, bytes) <- classFiles) newFileDict(name) = bytes
        Class.forName(wrapperName , true, evalClassloader)
      }, e => "Failed to load compiled class " + e)
    } yield (cls, importData)

    def evalMain(cls: Class[_]) =
      cls.getDeclaredMethod("$main").invoke(null)


    def previousImportBlock = {
      previousImports
        .values
        .groupBy(_.prefix)
        .map{
        case (prefix, Seq(imp)) if imp.fromName == imp.toName=>
          s"import $prefix.${BacktickWrap(imp.fromName)}"
        case (prefix, imports) =>
          val lines = for(x <- imports) yield {
            x.toName match{
              case None => "\n  " + BacktickWrap(x.fromName)
              case Some(toName) => "\n  " + BacktickWrap(x.fromName) + " => " + BacktickWrap(toName)
            }
          }
          val block = lines.mkString(",")
          s"import $prefix.{$block\n}"
        }
        .mkString("\n")
    }
    def interrupted() = {
      Thread.interrupted()
      Res.Failure("\nInterrupted!")
    }

    type InvEx = InvocationTargetException
    type InitEx = ExceptionInInitializerError

    def processLine(code: String, printCode: String, printer: Iterator[String] => Unit) = for {
      wrapperName <- Res.Success("cmd" + currentLine)
      (cls, newImports) <- evalClass(
        s"""
        $previousImportBlock

        object $wrapperName{
          $code
          def $$main() = {$printCode}
        }
        """,
        wrapperName
      )
      _ = currentLine += 1
      _ <- Catching{
        case Ex(_: InvEx, _: InitEx, ReplExit)  => Res.Exit
        case Ex(_: ThreadDeath)                 => interrupted()
        case Ex(_: InvEx, _: ThreadDeath)       => interrupted()
        case Ex(_: InvEx, _: InitEx, userEx@_*) => Res.Failure(userEx, stop = "$main")
        case Ex(userEx@_*)                      => Res.Failure(userEx, stop = "evaluatorRunPrinter")
      }
    } yield {
      // Exhaust the printer iterator now, before exiting the `Catching`
      // block, so any exceptions thrown get properly caught and handled
      evaluatorRunPrinter(printer(evalMain(cls).asInstanceOf[Iterator[String]]))
      Evaluated(
        wrapperName,
        newImports.map(id => id.copy(
          wrapperName = wrapperName,
          prefix = if (id.prefix == "") wrapperName else id.prefix
        ))
      )
    }

    def update(newImports: Seq[ImportData]) = {
      for(i <- newImports) previousImports(i.toName.getOrElse(i.fromName)) = i
    }
  }

  /**
   * Dummy function used to mark this method call in the stack trace,
   * so we can easily cut out the irrelevant part of the trace when
   * showing it to the user.
   */
  def evaluatorRunPrinter(f: => Unit) = f

}