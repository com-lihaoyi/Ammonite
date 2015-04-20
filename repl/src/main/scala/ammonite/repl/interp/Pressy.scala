package ammonite.repl.interp

import acyclic.file

import scala.reflect.internal.util.{OffsetPosition, BatchSourceFile}
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.interactive.Response
import scala.tools.nsc.util._

/**
 * Nice wrapper for the presentation compiler.
 */
trait Pressy{
  def complete(snippetIndex: Int, previousImports: String, snippet: String): (Int, Seq[String], Seq[String])
  def shutdownPressy(): Unit
}
object Pressy {

  /**
   * Encapsulates all the logic around a single instance of
   * `nsc.interactive.Global` and other data specific to a single completion
   */
  class Run(val pressy: nsc.interactive.Global,
            currentFile: BatchSourceFile,
            allCode: String,
            index: Int){
    val tree = pressy.parseTree(currentFile)
    def prefixed: Seq[(Int, Seq[(String, Option[String])])] = tree.collect {
      case t@pressy.Ident(name)
        if t.pos.startOrPoint <= index && index <= t.pos.endOrPoint =>
        val r = ask(index, pressy.askScopeCompletion)

        lazy val shallow = {
          pressy.ask(() => r.filter(_.sym.name.decoded.startsWith(name.decoded)).map{
            case x if x.sym.name.decoded == name.decoded =>
              (x.sym.name.decoded, Some(x.sym.defString))
            case x =>
              (x.sym.name.decoded, None)
          })
        }

        /**
         * Search for terms to autocomplete not just from the local scope,
         * but from any packages and package objects accessible from the
         * local scope
         */
        def rec(t: pressy.Symbol): Seq[pressy.Symbol] = {
          val children =
            if (t.hasPackageFlag || t.isPackageObject) {
              pressy.ask(() => t.typeSignature.members.filter(_ != t).flatMap(rec))
            } else Nil

          t +: children.toSeq
        }

        lazy val allDeep = pressy.ask(() =>
          for {
            member <- pressy.RootClass.typeSignature.members.toSeq
            sym <- rec(member)
            // sketchy name munging because I don't know how to do this properly
            strippedName = sym.nameString.stripPrefix("package$").stripSuffix("$")
            if strippedName.startsWith(name.decoded)
            (pref, suf) = sym.fullNameString.splitAt(sym.fullNameString.lastIndexOf('.') + 1)
            out = pref + strippedName
            if out != ""
          } yield (out, None)
        )

        lazy val deep = allDeep.distinct

        if (shallow.length > 0) (t.pos.startOrPoint, shallow)
        else if (deep.length == 1) (t.pos.startOrPoint, deep)
        else (t.pos.endOrPoint, deep :+ ("", None))
    }
    def ask(index: Int, query: (Position, Response[List[pressy.Member]]) => Unit) = {
      val (first, last) = allCode.splitAt(index)

      val position = new OffsetPosition(currentFile, index)
      val scopes = Compiler.awaitResponse[List[pressy.Member]](query(position, _))
      scopes.filter(_.accessible)
    }

    def dotted: Seq[(Int, Seq[(String, Option[String])])] = tree.collect {
      case t@pressy.Select(qualifier, name)
        if qualifier.pos.endOrPoint <= index && index <= t.pos.endOrPoint =>
        val r = ask(qualifier.pos.endOrPoint,  pressy.askTypeCompletion)
        val prefix = if (name.decoded == "<error>") "" else name.decoded
        (
          qualifier.pos.endOrPoint + 1,
          pressy.ask(() => r.filter(_.sym.name.decoded.startsWith(prefix)).map{
            case x if x.sym.name.decoded == prefix =>
              (x.sym.name.decoded, Some(x.sym.defString))
            case x =>
              (x.sym.name.decoded, None)
          })
          )
    }

    def scoped: (Int, List[(String, Option[String])]) = {
      index -> ask(index, pressy.askScopeCompletion).map(s =>
        pressy.ask(() => (s.sym.name.decoded, None))
      )
    }
  }
  def apply(jarDeps: Seq[java.io.File],
            dirDeps: Seq[java.io.File],
            dynamicClasspath: VirtualDirectory): Pressy = new Pressy {


    var cachedPressy: nsc.interactive.Global = null

    def initPressy = {
      val (settings, reporter, vd, jcp) = Compiler.initGlobalBits(
        jarDeps, dirDeps, dynamicClasspath, _ => (), scala.Console.YELLOW
      )
      new nsc.interactive.Global(settings, reporter) {
        g =>

        override def classPath = platform.classPath // Actually jcp, avoiding a path-dependent type issue in 2.10 here

        override lazy val platform: ThisPlatform = new JavaPlatform {
          val global: g.type = g

          override def classPath = jcp
        }
        override lazy val analyzer = CompilerCompatibility.interactiveAnalyzer(g)
      }
    }

    /**
     * Ask for autocompletion at a particular spot in the code, returning
     * possible things that can be completed at that location. May try various
     * different completions depending on where the `index` is placed, but
     * the outside caller probably doesn't care.
     */
    def complete(snippetIndex: Int, previousImports: String, snippet: String): (Int, Seq[String], Seq[String]) = {

      val prefix = previousImports + "\nobject AutocompleteWrapper{\n"
      val suffix = "\n}"
      val allCode =  prefix + snippet + suffix
      val index = snippetIndex + prefix.length
      if (cachedPressy == null) cachedPressy = initPressy

      val pressy = cachedPressy
      val currentFile = new BatchSourceFile(
        Compiler.makeFile(allCode.getBytes, name = "Current.scala"),
        allCode
      )

      val r = new Response[Unit]
      pressy.askReload(List(currentFile), r)

      r.get.fold(
        x => x,
        e => throw e
      )

      val run = new Run(pressy, currentFile, allCode, index)

      val (i, all) =
        run.dotted.headOption orElse run.prefixed.headOption getOrElse run.scoped

      val allNames = all.collect{ case (name, None) => name}.filter(_ != "<init>")
      val signatures = all.collect{ case (name, Some(defn)) => defn }

      (i - prefix.length, allNames, signatures)
    }

    def shutdownPressy() = {
      Option(cachedPressy).foreach(_.askShutdown())
      cachedPressy = null
    }
  }
}