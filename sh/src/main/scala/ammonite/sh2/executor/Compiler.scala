package ammonite.sh2.executor

import java.io._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.reflect.io
import scala.reflect.io._
import scala.tools.nsc
import scala.tools.nsc.Settings
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.interactive.Response
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.util.ClassPath.JavaContext
import scala.tools.nsc.util._

object Compiler{
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
 * Handles the interaction between scala-js-fiddle and
 * scalac/scalajs-tools to compile and optimize code submitted by users.
 */
class Compiler(dynamicClasspath: VirtualDirectory) {
  import ammonite.sh2.executor.Compiler._
  val blacklist = Seq("<init>")

  /**
   * Converts Scalac's weird Future type
   * into a standard scala.concurrent.Future
   */
  def toFuture[T](func: Response[T] => Unit): Future[T] = {
    val r = new Response[T]
    Future { func(r) ; r.get.left.get }
  }


  trait InMemoryGlobal { g: scala.tools.nsc.Global =>
    def ctx: JavaContext
    def dirs: IndexedSeq[ClassPath[AbstractFile]]
    override def classPath = new JavaClassPath(dirs, ctx)
    override lazy val platform: ThisPlatform = new JavaPlatform{
      val global: g.type = g
      override def classPath = new JavaClassPath(dirs, ctx)
    }

  }


  def compile(src: Array[Byte], logger: String => Unit = _ => ()): Option[Traversable[AbstractFile]] = {

    val singleFile = makeFile( src)

    val (settings, reporter, vd, jCtx, jDirs) = initGlobalBits(logger)
    val compiler = new nsc.Global(settings, reporter) with InMemoryGlobal{ g =>
      def ctx = jCtx
      def dirs = jDirs
    }

    val run = new compiler.Run()
    run.compileFiles(List(singleFile))

    if (vd.iterator.isEmpty) None
    else Some{
      for{
        x <- vd.iterator.to[collection.immutable.Traversable]
        if x.name.endsWith(".class")
      } yield {
        x
      }
    }
  }
  /**
   * Code to initialize random bits and pieces that are needed
   * for the Scala compiler to function, common between the
   * normal and presentation compiler
   */
  def initGlobalBits(logger: String => Unit)= {
    val vd = new io.VirtualDirectory("(memory)", None)
    lazy val settings = new Settings
    val jCtx = new JavaContext()
    val jDirs = Classpath.jarDeps.map(x =>
      new DirectoryClassPath(new FileZipArchive(x), jCtx)
    ).toVector ++ Classpath.dirDeps.map(x =>
      new DirectoryClassPath(new PlainDirectory(new Directory(x)), jCtx)
    ) ++ Seq(new DirectoryClassPath(dynamicClasspath, jCtx))

    settings.outputDirs.setSingleOutput(vd)
    val writer = new Writer{
      var inner = mutable.Buffer[Array[Char]]()
      def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
        inner += cbuf.slice(off, off + len)
      }
      def flush(): Unit = {
        logger(inner.flatten.mkString)
        inner = mutable.Buffer[Array[Char]]()
      }
      def close(): Unit = ()
    }
    val reporter = new ConsoleReporter(settings, scala.Console.in, new PrintWriter(writer))
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