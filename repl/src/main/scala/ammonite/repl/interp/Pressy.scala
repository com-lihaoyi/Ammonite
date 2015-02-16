package ammonite.repl.interp

import scala.reflect.internal.util.{OffsetPosition, BatchSourceFile}
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.interactive.{Response, InteractiveAnalyzer}
import scala.tools.nsc.util._

class Pressy(jarDeps: Seq[java.io.File],
             dirDeps: Seq[java.io.File],
             dynamicClasspath: VirtualDirectory) {
  var cachedPressy: nsc.interactive.Global = null

  /**
   * Ask for autocompletion at a particular spot in the code, returning
   * possible things that can be completed at that location. May try various
   * different completions depending on where the `index` is placed, but
   * the outside caller probably doesn't care.
   */
  def complete(index: Int, allCode: String): (Int, Seq[String]) = {
    if (cachedPressy == null){
      cachedPressy = {
        val (settings, reporter, vd, jcp) = Compiler.initGlobalBits(
          jarDeps, dirDeps, dynamicClasspath, _ => (), scala.Console.YELLOW
        )
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
    }

    val pressy = cachedPressy
    val currentFile = new BatchSourceFile(
      Compiler.makeFile(allCode.getBytes, name = "Current.scala"),
      allCode
    )

    /**
     * Queries the presentation compiler for a list of members
     */
    def askPressy(index: Int,
                  query: (Position, Response[List[pressy.Member]]) => Unit) = {

      val position = new OffsetPosition(currentFile, index)
      val scopes = Compiler.awaitResponse[List[pressy.Member]](query(position, _))
      scopes.filter(_.accessible)
    }

    val file = new BatchSourceFile(
      Compiler.makeFile(allCode.getBytes, name = "Hello.scala"),
      allCode
    )

    val r = new Response[Unit]
    pressy.askReload(List(currentFile), r)

    r.get.fold(
      x => x,
      e => throw e
    )

    def ask(index: Int, query: (Position, Response[List[pressy.Member]]) => Unit) = {
      val (first, last) = allCode.splitAt(index)
      askPressy(index, query)
    }
    val tree = pressy.parseTree(file)

    def dotted = tree.collect{
      case t @ pressy.Select(qualifier, name)
        if qualifier.pos.end <= index && index <= t.pos.end =>
        val r = ask(qualifier.pos.end, pressy.askTypeCompletion)
        val prefix = if(name.decoded == "<error>") "" else name.decoded
        (qualifier.pos.end + 1, pressy.ask(() => r.map(_.sym.name.decoded).filter(_.startsWith(prefix))))
    }

    def prefixed = tree.collect{
      case t @ pressy.Ident(name)
        if t.pos.start <= index && index <= t.pos.end =>
        val r = ask(index, pressy.askScopeCompletion)

        lazy val shallow = {
          pressy.ask(() => r.map(_.sym.name.decoded).filter(_.startsWith(name.decoded)))
        }

        /**
         * Search for terms to autocomplete not just from the local scope,
         * but from any packages and package objects accessible from the
         * local scope
         */
        def rec(t: pressy.Symbol): Seq[pressy.Symbol] = {
          val children =
            if (t.hasPackageFlag || t.isPackageObject){
              t.typeSignature.members.filter(_ != t).flatMap(rec)
            } else Nil

          t +: children.toSeq
        }

        lazy val allDeep = pressy.ask(() =>
          for{
            member <- pressy.RootClass.typeSignature.members.toSeq
            sym <- rec(member)
            // sketchy name munging because I don't know how to do this properly
            strippedName = sym.nameString.stripPrefix("package$").stripSuffix("$")
            if strippedName.startsWith(name.decoded)
            (pref, suf) = sym.fullNameString.splitAt(sym.fullNameString.lastIndexOf('.') + 1)
            out = pref + strippedName
            if out != ""
          } yield out
        )

        lazy val deep = allDeep.distinct

        if (shallow.length > 0) (t.pos.start, shallow)
        else if (deep.length == 1) (t.pos.start, deep)
        else (t.pos.end, deep :+ "")
    }

    def scoped = {
      index -> ask(index, pressy.askScopeCompletion).map(s =>
        pressy.ask(() => s.sym.name.decoded)
      )
    }

    val (i, all) = dotted.headOption orElse prefixed.headOption getOrElse scoped

    (i, all.filter(_ != "<init>"))
  }

  def shutdownPressy() = {
    Option(cachedPressy).foreach(_.askShutdown())
    cachedPressy = null
  }
}
