package ammonite.repl.interp

import acyclic.file

import scala.reflect.internal.util.{Position, OffsetPosition, BatchSourceFile}
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.interactive.Response
import scala.tools.nsc.util.Position
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
    val r = new Response[pressy.Tree]
    pressy.askTypeAt(Position.offset(currentFile, index), r)
    val tree = r.get.fold(x => x, e => throw e)
    /**
     * Search for terms to autocomplete not just from the local scope,
     * but from any packages and package objects accessible from the
     * local scope
     */
    def deepCompletion(name: String) = {
      def rec(t: pressy.Symbol): Seq[pressy.Symbol] = {
        val children =
          if (t.hasPackageFlag || t.isPackageObject) {
            pressy.ask(() => t.typeSignature.members.filter(_ != t).flatMap(rec))
          } else Nil

        t +: children.toSeq
      }

      pressy.ask(() =>
        for {
          member <- pressy.RootClass.typeSignature.members.toList
          sym <- rec(member)
          // sketchy name munging because I don't know how to do this properly
          strippedName = sym.nameString.stripPrefix("package$").stripSuffix("$")
          if strippedName.startsWith(name)
          (pref, _) = sym.fullNameString.splitAt(sym.fullNameString.lastIndexOf('.') + 1)
          out = pref + strippedName
          if out != ""
        } yield (out, None)
      )
    }
    def handleTypeCompletion(position: Int, decoded: String, offset: Int) = {

      val r = ask(position,  pressy.askTypeCompletion)
      val prefix = if (decoded == "<error>") "" else decoded
      (position + offset, handleCompletion(r, prefix))
    }

    def handleCompletion(r: List[pressy.Member], prefix: String) = {
      pressy.ask{() => r.filter(_.sym.name.decoded.startsWith(prefix)).map{
        case x if x.sym.name.decoded == prefix =>
          (x.sym.name.decoded, Some(x.sym.defString))
        case x =>
          (x.sym.name.decoded, None)
      }}
    }

    def prefixed: (Int, Seq[(String, Option[String])]) = tree match {
      case t @ pressy.Select(qualifier, name) =>
        val dotOffset = if (qualifier.pos.point == t.pos.point) 0 else 1
        handleTypeCompletion(qualifier.pos.end, name.decoded, dotOffset)

      case t @ pressy.Import(expr, selectors)  =>
        // If the selectors haven't been defined yet...
        if (selectors.head.name.toString == "<error>") {
          if (expr.tpe.toString == "<error>") {
            // If the expr is badly typed, try to scope complete it
            val exprName = expr.asInstanceOf[pressy.Ident].name.decoded
            expr.pos.point -> handleCompletion(
              ask(expr.pos.point, pressy.askScopeCompletion),
              // if it doesn't have a name at all, accept anything
              if (exprName == "<error>") "" else exprName
            )
          } else {
            // If the expr is well typed, type complete
            // the next thing
            handleTypeCompletion(expr.pos.end, "", 1)
          }
        }else {// I they're been defined, just use typeCompletion
          handleTypeCompletion(selectors.last.namePos, selectors.last.name.decoded, 0)
        }
      case t @ pressy.Ident(name) =>
        lazy val shallow = handleCompletion(
          ask(index, pressy.askScopeCompletion),
          name.decoded
        )
        lazy val deep = deepCompletion(name.decoded).distinct

        if (shallow.length > 0) (t.pos.start, shallow)
        else if (deep.length == 1) (t.pos.start, deep)
        else (t.pos.end, deep :+ ("" -> None))

      case t =>
        index -> ask(index, pressy.askScopeCompletion).map { s =>
          pressy.ask(() => (s.sym.name.decoded, None))
        }
    }
    def ask(index: Int, query: (Position, Response[List[pressy.Member]]) => Unit) = {
      val position = new OffsetPosition(currentFile, index)
      val scopes = Compiler.awaitResponse[List[pressy.Member]](query(position, _))
      scopes.filter(_.accessible)
    }

  }
  def apply(jarDeps: Seq[java.io.File],
            dirDeps: Seq[java.io.File],
            dynamicClasspath: VirtualDirectory,
            evalClassloader: => ClassLoader): Pressy = new Pressy {

    var cachedPressy: nsc.interactive.Global = null

    def initPressy = {
      val (settings, reporter, _, jcp) = Compiler.initGlobalBits(
        jarDeps, dirDeps, dynamicClasspath, _ => (), scala.Console.YELLOW
      )
      new nsc.interactive.Global(settings, reporter) {
        g =>

        override def classPath = platform.classPath // Actually jcp, avoiding a path-dependent type issue in 2.10 here

        override lazy val platform: ThisPlatform = new JavaPlatform {
          val global: g.type = g

          override def classPath = jcp
        }
        override lazy val analyzer = CompilerCompatibility.interactiveAnalyzer(g, evalClassloader)
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
      r.get.fold(x => x, e => throw e)

      val run = new Run(pressy, currentFile, allCode, index)

      val (i, all) = run.prefixed

      val allNames = all.collect{ case (name, None) => name}
                        .filter(_ != "<init>")
                        .filter(!_.contains('$'))
      val signatures = all.collect{ case (name, Some(defn)) => defn }

      (i - prefix.length, allNames, signatures)
    }

    def shutdownPressy() = {
      Option(cachedPressy).foreach(_.askShutdown())
      cachedPressy = null
    }
  }
}
