package ammonite.repl.interp

import java.io.{File, FileOutputStream}
import java.lang.reflect.InvocationTargetException
import java.net.URL
import java.util.UUID

import acyclic.file
import ammonite.repl._
import java.net.URLClassLoader
import scala.reflect.runtime.universe._
import scala.collection.mutable
import scala.util.Try
import scala.util.control.ControlThrowable

/**
 * Takes source code and, with the help of a compiler and preprocessor,
 * evaluates it and returns a `Result[(output: String, imports: String)]`
 * where `output` is what gets printed and `imports` are any imports that
 * need to get prepended to subsequent commands.
 *
 * @tparam A: preprocessor output type
 * @tparam B: wrapper $main method return type
 */
trait Evaluator[-A, +B] {
  def evalClass(code: String, wrapperName: String, useClassWrapper: Boolean = false): Res[(Class[_], Class[_], Seq[ImportData])]
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
  def processLine[C](input: A, process: B => C, useClassWrapper: Boolean = false): Res[Evaluated[C]]

  def previousImportBlock: String
  def addJar(url: URL): Unit
  def newClassloader(): Unit
  def evalClassloader: ClassLoader
}

object Evaluator{
  /**
   * Thrown to exit the Evaluator cleanly
   */
  case object Exit extends ControlThrowable

  def namesFor(t: scala.reflect.runtime.universe.Type): Set[String] = {
    val yours = t.members.map(_.name.toString).toSet
    val default = typeOf[Object].members.map(_.name.toString)
    yours -- default
  }

  def namesFor[T: TypeTag]: Set[String] = namesFor(typeOf[T])

  def apply[A, B](currentClassloader: ClassLoader,
                  initialImports: Seq[(String, ImportData)],
                  preprocess: (String, Int) => Res[A],
                  wrap: (A, String, String) => String,
                  compile: => (Array[Byte], String => Unit) => Compiler.Output,
                  stdout: String => Unit): Evaluator[A, B] = new Evaluator[A, B] {

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
    lazy val previousImports = mutable.Map(initialImports: _*)


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

    lazy val tmpClassDir = {
      val d = new File(new File(System.getProperty("java.io.tmpdir")), s"ammonite-${UUID.randomUUID()}")
      d.mkdirs()
      d.deleteOnExit()
      d
    }

    def newClassloader() = {
      evalClassloader = new URLClassLoader(Array(), evalClassloader){
        def add(url: URL) = addURL(url)
        override def getResource(name: String): URL = {
          if (name.endsWith(".class") && newFileDict.contains(name.stripSuffix(".class"))) {
            val f = new File(tmpClassDir, name)
            if (!f.exists()) {
              val w = new FileOutputStream(f)
              w.write(newFileDict(name.stripSuffix(".class")))
              w.close()
            }

            f.toURI.toURL
          } else
            super.getResource(name)
        }
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

    def evalClass(code: String, wrapperName: String, useClassWrapper: Boolean = false) = for{

      (output, compiled) <- Res.Success{
        val output = mutable.Buffer.empty[String]
        val c = compile(code.getBytes, output.append(_))
        (output, c)
      }

      (classFiles, importData) <- Res[(Traversable[(String, Array[Byte])], Seq[ImportData])](
        compiled, "Compilation Failed\n" + output.mkString("\n")
      )

      (cls, objCls) <- Res[(Class[_], Class[_])](Try {
        for ((name, bytes) <- classFiles) newFileDict(name) = bytes
        val cls = Class.forName(wrapperName, true, evalClassloader)
        val objCls = if (useClassWrapper) Class.forName(wrapperName + "$", true, evalClassloader) else null
        (cls, objCls)
      }, e => "Failed to load compiled class " + e)
    } yield (cls, objCls, importData)

    def evalMain(cls: Class[_], instance: AnyRef) =
      cls.getDeclaredMethod("$main").invoke(instance)

    def transpose[A](xs: List[List[A]]): List[List[A]] = xs.filter(_.nonEmpty) match {
      case Nil    =>  Nil
      case ys: List[List[A]] => ys.map{ _.head }::transpose(ys.map{ _.tail })
    }
    def previousImportBlock = {
      val snippets = for {
        (prefix, allImports) <- previousImports.values.toList.groupBy(_.prefix)
        imports <- transpose(allImports.groupBy(_.fromName).values.toList)
      } yield {
        imports match{
          case Seq(imp) if imp.fromName == imp.toName =>
            s"import $prefix.${BacktickWrap(imp.fromName)}"
          case imports =>
            val lines = for (x <- imports) yield {
              if (x.fromName == x.toName)
                "\n  " + BacktickWrap(x.fromName)
              else
                "\n  " + BacktickWrap(x.fromName) + " => " + BacktickWrap(x.toName)

            }
            val block = lines.mkString(",")
            s"import $prefix.{$block\n}"
        }
      }
      snippets.mkString("\n")
    }
    def interrupted() = {
      Thread.interrupted()
      Res.Failure("\nInterrupted!")
    }

    type InvEx = InvocationTargetException
    type InitEx = ExceptionInInitializerError

    def processLine[C](input: A, process: B => C, useClassWrapper: Boolean = false) = for {
      wrapperName <- Res.Success("cmd" + currentLine)
      (cls, objClass, newImports) <- evalClass(wrap(input, previousImportBlock, wrapperName), wrapperName, useClassWrapper)
      _ = currentLine += 1
      _ <- Catching{
        case Ex(_: InvEx, _: InitEx, Exit)  => Res.Exit
        case Ex(_: ThreadDeath)                 => interrupted()
        case Ex(_: InvEx, _: ThreadDeath)       => interrupted()
        case Ex(_: InvEx, _: InitEx, userEx@_*) => Res.Failure(userEx, stop = "$main")
        case Ex(userEx@_*)                      => Res.Failure(userEx, stop = "evaluatorRunPrinter")
      }
    } yield {
      // Exhaust the printer iterator now, before exiting the `Catching`
      // block, so any exceptions thrown get properly caught and handled
      val value = evaluatorRunPrinter(process {
        val instance =
          if (useClassWrapper)
            objClass getField "MODULE$" get null
          else
            null

        evalMain(cls, instance).asInstanceOf[B]
      })
      Evaluated(
        wrapperName,
        newImports.map(id => id.copy(
          wrapperName = wrapperName,
          prefix = if (id.prefix == "") wrapperName else id.prefix
        )),
        value
      )
    }

    def update(newImports: Seq[ImportData]) = {
      for(i <- newImports) previousImports(i.toName) = i
    }
  }

  /**
   * Dummy function used to mark this method call in the stack trace,
   * so we can easily cut out the irrelevant part of the trace when
   * showing it to the user.
   */
  def evaluatorRunPrinter[T](f: => T): T = f

}
