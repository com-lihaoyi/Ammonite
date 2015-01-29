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
import concurrent.duration._
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
  def awaitResponse[T](func: Response[T] => Unit): T = {
    val r = new Response[T]
    func(r)
    r.get.fold(
      x => x,
      e => throw e
    )
  }

  var currentLogger: String => Unit = s => ()

  val pressy = {
    val (settings, reporter, vd, jcp) = initGlobalBits(currentLogger)
    new nsc.interactive.Global(settings, reporter) { g =>
      override def classPath = jcp
      override lazy val platform: ThisPlatform = new JavaPlatform{
        val global: g.type = g
        override def classPath = jcp
      }
    }

  }
  val (vd, reporter, compiler) = {
    val (settings, reporter, vd, jcp) = initGlobalBits(currentLogger)
    val scalac = new nsc.Global(settings, reporter) { g =>
      override def classPath = jcp
      override lazy val platform: ThisPlatform = new JavaPlatform{
        val global: g.type = g
        override def classPath = jcp
      }
      override lazy val analyzer = new { val global: g.type = g } with Analyzer {
        override def findMacroClassLoader() = new ClassLoader(this.getClass.getClassLoader){}
      }
    }
    (vd, reporter, scalac)
  }

  var i = 0
  def askCustomImports(wrapperName: String, allCode: String): Seq[(String, String)] = {
    println("---askCustomImports---")
    println(allCode)
    i+=1
    val filename = "asd" + i + ".scala"
    val file = new BatchSourceFile(
      makeFile(
        allCode.getBytes,
        name=filename
      ),
      allCode
    )

    println("---askReload---")
    awaitResponse[Unit](pressy.askReload(List(file), _))

    def askAt(index: Int) = {
      println(s"askAt($index)")
      val position = new OffsetPosition(file, index)
      val scopes = awaitResponse[List[pressy.Member]](pressy.askScopeCompletion(position, _)).toSet
//      val types = awaitResponse[List[pressy.Member]](pressy.askTypeCompletion(position, _)).toSet
      scopes.filter(_.accessible)
    }

    val end = askAt(allCode.lastIndexOf('}') - 1).map(n => n.sym.name.toString -> n).toMap

    val res = pressy.ask { () =>
      end.collect{
        case (k, x: pressy.ScopeMember)
        if x.sym.owner.toString != "class Object"
        && x.sym.owner.toString != "class Any"
        && x.sym.owner.toString != "package <root>"
        && x.sym.owner.toString != "package <empty>"
        && k != "<init>"
        && k != "_root_"
        && k != "<repeated...>"
        && k != "<repeated>"
        && k != "<byname>"
        && k != "package"
        && !(
          x.viaImport.toString == "import <empty>" &&
          x.sym.owner.toString == "package <empty>"
        ) =>
          val importTxt =
            x.viaImport
             .toString
             .padTo(30, ' ')
             .replace(".this.", ".")
             .replace("<empty>", wrapperName)

          k -> ("import " + importTxt)
      }
    }.toSeq
    val (first, last) = res.partition(_._2 != "import " + wrapperName)
    first ++ last
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
    val jcp = new JavaClassPath(jDirs, jCtx)
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
    (settings, reporter, vd, jcp)
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
