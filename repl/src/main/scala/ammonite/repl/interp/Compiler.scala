package ammonite.repl.interp


import acyclic.file
import ammonite.repl.{ImportData, Parsed}
import scala.collection.mutable
import scala.reflect.internal.util.{BatchSourceFile, OffsetPosition, Position}
import scala.reflect.io
import scala.reflect.io._
import scala.tools.nsc
import scala.tools.nsc.{Phase, Global, Settings}
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.interactive.{InteractiveAnalyzer, Response}
import scala.tools.nsc.plugins.{PluginComponent, Plugin}

import scala.tools.nsc.reporters.AbstractReporter
import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.util.ClassPath.JavaContext
import scala.tools.nsc.util.Position
import scala.tools.nsc.util._

object Compiler{
  /**
   * If the Option is None, it means compilation failed
   * Otherwise it's a Traversable of (filename, bytes) tuples
   */
  type Output = Option[(Traversable[(String, Array[Byte])], Seq[ImportData])]
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
 * Encapsulates (almost) all the ickiness of Scalac so it doesn't leak into
 * the rest of the codebase. Makes use of a good amount of mutable state
 * for things like the log-output-forwarder or compiler-plugin-output because
 * These things are hard-coded into Scalac and can't be passed in from run to
 * run.
 *
 * Turns source-strings into the bytes of classfiles, possibly more than one
 * classfile per source-string (e.g. inner classes, or lambdas). Also lets
 * you query source strings using an in-built presentation compiler
 */
class Compiler(jarDeps: Seq[java.io.File],
               dirDeps: Seq[java.io.File],
               dynamicClasspath: VirtualDirectory) {
  import ammonite.repl.interp.Compiler._

  var logger: String => Unit = s => ()
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

  val pressy = {
    val (settings, reporter, vd, jcp) = initGlobalBits(_ => (), scala.Console.YELLOW)
    new nsc.interactive.Global(settings, reporter) { g =>

      override def classPath = jcp
      override lazy val platform: ThisPlatform = new JavaPlatform{
        val global: g.type = g

        override def classPath = jcp
      }
      override lazy val analyzer = new { val global: g.type = g } with InteractiveAnalyzer {
        override def findMacroClassLoader() = new ClassLoader(this.getClass.getClassLoader){}
      }
    }
  }

  var lastImports = Seq.empty[ImportData]
  val (vd, reporter, compiler) = {
    val (settings, reporter, vd, jcp) = initGlobalBits(logger, scala.Console.RED)
    val scalac = new nsc.Global(settings, reporter) { g =>
      override lazy val plugins = List(new AmmonitePlugin(g, lastImports = _))
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


  /**
   * Ask for autocompletion at a particular spot in the code, returning
   * possible things that can be completed at that location. May try various
   * different completions depending on where the `index` is placed, but
   * the outside caller probably doesn't care.
   */
  def complete(index: Int, allCode: String): (Int, Seq[String]) = {
    val file = new BatchSourceFile(
      Compiler.makeFile(allCode.getBytes, name = "Hello.scala"),
      allCode
    )
    prepPressy(allCode)
    def ask(query: (Position, Response[List[pressy.Member]]) => Unit) = {
      askPressy(index, query)
    }
    val tree = pressy.parseTree(file)
    def dotted = tree.collect{
      case t @ pressy.Select(qualifier, name)
        if qualifier.pos.end <= index && index <= t.pos.end =>
        val r = ask(pressy.askTypeCompletion)
        val prefix = if(name.decoded == "<error>") "" else name.decoded
        (qualifier.pos.end + 1, pressy.ask(() => r.map(_.sym.name.decoded).filter(_.startsWith(prefix))))
    }

    def prefixed = tree.collect{
      case t @ pressy.Ident(name)
        if t.pos.start <= index && index <= t.pos.end =>
        val r = ask(pressy.askScopeCompletion)
        (t.pos.start, pressy.ask(() => r.map(_.sym.name.decoded).filter(_.startsWith(name.decoded))))
    }

    def scoped = {
      index -> ask(pressy.askScopeCompletion)
        .map(s => pressy.ask(() => s.sym.name.decoded))

    }

    val (i, all) = dotted.headOption orElse prefixed.headOption getOrElse scoped
    (i, all.filter(_ != "<init>"))
  }

  var currentFile: BatchSourceFile = null
  def primePressy(allCode: String) = {
    currentFile = new BatchSourceFile(
      makeFile(allCode.getBytes, name = "Current.scala"),
      allCode
    )
    val r = new Response[Unit]

    pressy.askReload(List(currentFile), r)
    r
  }
  def prepPressy(allCode: String) = {
    primePressy(allCode).get.fold(
      x => x,
      e => throw e
    )
  }
  /**
   * Queries the presentation compiler for a list of members
   */
  def askPressy(index: Int,
                query: (Position, Response[List[pressy.Member]]) => Unit) = {

    val position = new OffsetPosition(currentFile, index)
    val scopes = awaitResponse[List[pressy.Member]](query(position, _))
    scopes.filter(_.accessible)
  }

  /**
   * Compiles a blob of bytes and spits of a list of classfiles
   */
  def compile(src: Array[Byte], runLogger: String => Unit): Output = {
    compiler.reporter.reset()
    this.logger = runLogger
    val singleFile = makeFile( src)

    val run = new compiler.Run()
    vd.clear()
    run.compileFiles(List(singleFile))
    if (reporter.hasErrors) None
    else Some{
      val files = for{
        x <- vd.iterator.to[collection.immutable.Traversable]
        if x.name.endsWith(".class")
      } yield {
        val output = dynamicClasspath.fileNamed(x.name).output
        output.write(x.toByteArray)
        output.close()
        (x.name.stripSuffix(".class"), x.toByteArray)
      }
      val imports = lastImports.toList
      (files, imports)
    }
  }

  /**
   * Weird hack, stolen from ILoop, used to parse snippets of code while
   * letting us know if it's incomplete (vs just blowing up)
   */
  def parse(line: String): Parsed = {
    var isIncomplete = false

    val out = mutable.Buffer.empty[String]
    logger = out.append(_)
    val r = compiler.currentRun

    val p = r.parsing
    p.withIncompleteHandler((_, _) => isIncomplete = true) {
      reporter.reset()
      val trees = compiler.newUnitParser(line).parseStats()
      if (reporter.hasErrors) Parsed.Error(out.mkString("\n"))
      else if (isIncomplete) Parsed.Incomplete
      else Parsed.Success(trees)
    }
  }

  /**
   * Code to initialize random bits and pieces that are needed
   * for the Scala compiler to function, common between the
   * normal and presentation compiler
   */
  def initGlobalBits(logger: => String => Unit, errorColor: String)= {
    val vd = new io.VirtualDirectory("(memory)", None)
    lazy val settings = new Settings
    val settingsX = settings
    settingsX.Yrangepos.value = true
    val jCtx = new JavaContext()
    val jDirs = jarDeps.map(x =>
      new DirectoryClassPath(new FileZipArchive(x), jCtx)
    ).toVector ++ dirDeps.map(x =>
      new DirectoryClassPath(new PlainDirectory(new Directory(x)), jCtx)
    ) ++ Seq(new DirectoryClassPath(dynamicClasspath, jCtx))
    val jcp = new JavaClassPath(jDirs, jCtx)
    settings.outputDirs.setSingleOutput(vd)

    val reporter = new AbstractReporter {
      def displayPrompt(): Unit = ???

      def display(pos: Position, msg: String, severity: Severity) = {
        severity match{
          case ERROR => logger(
            errorColor + Position.formatMessage(pos, msg, false) + scala.Console.RESET
          )
          case _ => logger(msg)
        }
      }

      val settings = settingsX
    }
    (settings, reporter, vd, jcp)
  }
}
