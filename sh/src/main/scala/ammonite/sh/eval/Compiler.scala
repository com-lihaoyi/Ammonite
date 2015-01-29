package ammonite.sh.eval

import java.io._
import acyclic.file
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.reflect.internal.util.{OffsetPosition, Position}
import scala.reflect.io
import scala.reflect.io._
import scala.tools.nsc
import scala.tools.nsc.Settings
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.interactive.{InteractiveAnalyzer, Response}
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.{AbstractReporter, Reporter, ConsoleReporter}
import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.util.ClassPath.JavaContext
import scala.tools.nsc.util.Position
import scala.tools.nsc.util._

object Compiler{
  /**
   * If the Option is None, it means compilation failed
   * Otherwise it's a Traversable of (filename, bytes) tuples
   */
  type Output = Option[Traversable[(String, Array[Byte])]]
  /**
   * Converts a bunch of bytes into Scalac's weird VirtualFile class
   */
  def makeFile(src: Array[Byte], name: String = "Main.scala") = {
    val singleFile = new io.VirtualFile(name)
    val output = singleFile.output
    output.write(src)
    output.close()
    singleFile
  }
}
/**
 * Turns source-strings into the bytes of classfiles, possibly more than one
 * classfile per source-string (e.g. inner classes, or lambdas)
 */
class Compiler(dynamicClasspath: VirtualDirectory) {
  import ammonite.sh.eval.Compiler._
  val blacklist = Seq("<init>")

  /**
   * Converts Scalac's weird Future type
   * into a standard scala.concurrent.Future
   */
  def toFuture[T](func: Response[T] => Unit): Future[T] = {
    val r = new Response[T]
    Future { func(r) ; r.get.left.get }
  }

  var currentLogger: String => Unit = s => ()
  val (settings, reporter, vd, jCtx, jDirs) = initGlobalBits(currentLogger)

  trait InMemGlobal{ g: nsc.Global =>
    override lazy val platform: ThisPlatform = new JavaPlatform{
      val global: g.type = g
      override def classPath = new JavaClassPath(jDirs, jCtx)
    }
  }

  val pressy = new nsc.interactive.Global(settings, reporter) with InMemGlobal

  val compiler = new nsc.Global(settings, reporter) with InMemGlobal { g =>
    override lazy val analyzer = new { val global: g.type = g } with Analyzer {
      override def findMacroClassLoader() = new ClassLoader(this.getClass.getClassLoader) {}
    }
  }

  def compile(src: Array[Byte], logger: String => Unit = _ => ()): Output = {
    compiler.reporter.reset()
    currentLogger = logger
    val singleFile = makeFile( src)

    val run = new compiler.Run()
    vd.clear()
    run.compileFiles(List(singleFile))
    if (reporter.hasErrors) None
    else Some{
      for{
        x <- vd.iterator.to[collection.immutable.Traversable]
        if x.name.endsWith(".class")
      } yield {
        val output = dynamicClasspath.fileNamed(x.name).output
        output.write(x.toByteArray)
        output.close()
        (x.name.stripSuffix(".class"), x.toByteArray)
      }
    }
  }

  /**
   * Code to initialize random bits and pieces that are needed
   * for the Scala compiler to function, common between the
   * normal and presentation compiler
   */
  def initGlobalBits(logger: => String => Unit)= {
    val vd = new io.VirtualDirectory("(memory)", None)
    lazy val settings = new Settings
    val settingsX = settings
    val jCtx = new JavaContext()
    val jDirs = Classpath.jarDeps.map(x =>
      new DirectoryClassPath(new FileZipArchive(x), jCtx)
    ).toVector ++ Classpath.dirDeps.map(x =>
      new DirectoryClassPath(new PlainDirectory(new Directory(x)), jCtx)
    ) ++ Seq(new DirectoryClassPath(dynamicClasspath, jCtx))

    settings.outputDirs.setSingleOutput(vd)

    val reporter = new AbstractReporter {
      def displayPrompt(): Unit = ???

      def display(pos: Position, msg: String, severity: Severity) = {
        severity match{
          case ERROR => logger(
            scala.Console.RED + Position.formatMessage(pos, msg, false) + scala.Console.RESET
          )
          case _ => logger(msg)
        }
      }

      val settings = settingsX
    }
    (settings, reporter, vd, jCtx, jDirs)
  }
//
//  def autocomplete(code: String, flag: String, pos: Int): Future[List[(String, String)]] = async {
//    // global can be reused, just create new runs for new compiler invocations
//    val (settings, reporter, vd, jCtx, jDirs) = initGlobalBits(_ => ())
//    val compiler = new nsc.interactive.Global(settings, reporter) with InMemoryGlobal { g =>
//      def ctx = jCtx
//      def dirs = jDirs
//      override lazy val analyzer = new {
//        val global: g.type = g
//      } with InteractiveAnalyzer {
//        override def findMacroClassLoader() = inMemClassloader
//      }
//    }
//
//    val file      = new BatchSourceFile(makeFile(code.getBytes), code)
//    val position  = new OffsetPosition(file, pos + Shared.prelude.length)
//
//    await(toFuture[Unit](compiler.askReload(List(file), _)))
//
//    val maybeMems = await(toFuture[List[compiler.Member]](flag match{
//      case "scope" => compiler.askScopeCompletion(position, _: compiler.Response[List[compiler.Member]])
//      case "member" => compiler.askTypeCompletion(position, _: compiler.Response[List[compiler.Member]])
//    }))
//
//    val res = compiler.ask{() =>
//      def sig(x: compiler.Member) = {
//        Seq(
//          x.sym.signatureString,
//          s" (${x.sym.kindString})"
//        ).find(_ != "").getOrElse("--Unknown--")
//      }
//      maybeMems.map((x: compiler.Member) => sig(x) -> x.sym.decodedName)
//        .filter(!blacklist.contains(_))
//        .distinct
//    }
//    compiler.askShutdown()
//    res
//  }

}