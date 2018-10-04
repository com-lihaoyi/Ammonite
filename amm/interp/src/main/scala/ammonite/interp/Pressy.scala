package ammonite.interp

import java.io.{OutputStream, PrintWriter}

import ammonite.interp.Compiler.makeReporter

import scala.reflect.internal.util.{BatchSourceFile, OffsetPosition, Position}
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Response
import scala.util.{Failure, Success, Try}
import ammonite.util.Util.newLine
/**
 * Nice wrapper for the presentation compiler.
 */
trait Pressy{
  /**
    * Ask for autocompletion at a particular spot in the code, returning
    * possible things that can be completed at that location. May try various
    * different completions depending on where the `index` is placed, but
    * the outside caller probably doesn't care.
    *
    * The returned index gives the position from which the possible replacements
    * should be inserted.
    */
  def complete(snippetIndex: Int,
               previousImports: String,
               snippet: String): (Int, Seq[String], Seq[String])

  /**
    * Basic type info at the provided index.
    */
  def info(snippetIndex: Int, previousImports: String, snippet: String): Try[String]
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

    /**
     * Dumb things that turn up in the autocomplete that nobody needs or wants
     */
    def blacklisted(s: pressy.Symbol) = {
      val blacklist = Set(
        "scala.Predef.any2stringadd.+",
        "scala.Any.##",
        "java.lang.Object.##",
        "scala.<byname>",
        "scala.<empty>",
        "scala.<repeated>",
        "scala.<repeated...>",
        "scala.Predef.StringFormat.formatted",
        "scala.Predef.Ensuring.ensuring",
        "scala.Predef.ArrowAssoc.->",
        "scala.Predef.ArrowAssoc.→",
        "java.lang.Object.synchronized",
        "java.lang.Object.ne",
        "java.lang.Object.eq",
        "java.lang.Object.wait",
        "java.lang.Object.notifyAll",
        "java.lang.Object.notify"
      )

      blacklist(s.fullNameAsName('.').decoded) ||
      s.isImplicit ||
      // Cache objects, which you should probably never need to
      // access directly, and apart from that have annoyingly long names
      "cache[a-f0-9]{32}".r.findPrefixMatchOf(s.name.decoded).isDefined ||
      s.isDeprecated ||
      s.decodedName == "<init>" ||
      s.decodedName.contains('$')
    }

    private val memberToString = {
      // Some hackery here to get at the protected CodePrinter.printedName which was the only
      // mildly reusable prior art I could locate. Other related bits:
      // - When constructing Trees, Scala captures the back-quoted nature into the AST as
      //   Ident.isBackquoted. Code-completion is inherently "pre-Tree", at least for the
      //   symbols being considered for use as completions, so not clear how this would be
      //   leveragable without big changes inside nsc.
      // - There's a public-but-incomplete implementation of rule-based backquoting in
      //   Printers.quotedName
      val nullOutputStream = new OutputStream() { def write(b: Int): Unit = {} }
      val backQuoter = new pressy.CodePrinter(
        new PrintWriter(nullOutputStream),
        printRootPkg = false
      ) {
        def apply(decodedName: pressy.Name): String = printedName(decodedName, decoded = true)
      }

      (member: pressy.Member) => {
        import pressy._
        // nsc returns certain members w/ a suffix (LOCAL_SUFFIX_STRING, " ").
        // See usages of symNameDropLocal in nsc's PresentationCompilerCompleter.
        // Several people have asked that Scala mask this implementation detail:
        // https://github.com/scala/bug/issues/5736
        val decodedName = member.sym.name.dropLocal.decodedName
        backQuoter(decodedName)
      }
    }

    val r = new Response[pressy.Tree]
    pressy.askTypeAt(new OffsetPosition(currentFile, index), r)
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

      for {
        member <- pressy.RootClass.typeSignature.members.toList
        sym <- rec(member)
        // sketchy name munging because I don't know how to do this properly
        // Note lack of back-quoting support.
        strippedName = sym.nameString.stripPrefix("package$").stripSuffix("$")
        if strippedName.startsWith(name)
        (pref, _) = sym.fullNameString.splitAt(sym.fullNameString.lastIndexOf('.') + 1)
        out = pref + strippedName
        if out != ""
      } yield (out, None)
    }
    def handleTypeCompletion(position: Int, decoded: String, offset: Int) = {

      val r = ask(position,  pressy.askTypeCompletion)
      val prefix = if (decoded == "<error>") "" else decoded
      (position + offset, handleCompletion(r, prefix))
    }

    def handleCompletion(r: List[pressy.Member], prefix: String) = pressy.ask{ () =>
      r.filter(_.sym.name.decoded.startsWith(prefix))
        .filter(m => !blacklisted(m.sym))
        .map{ x  =>
          (
            memberToString(x),
            if (x.sym.name.decoded != prefix) None
            else Some(x.sym.defString)
          )
        }
    }

    def prefixed: (Int, Seq[(String, Option[String])]) = tree match {
      case t @ pressy.Select(qualifier, name) =>

        val dotOffset = if (qualifier.pos.point == t.pos.point) 0 else 1

        //In scala 2.10.x if we call pos.end on a scala.reflect.internal.util.Position
        //that is not a range, a java.lang.UnsupportedOperationException is thrown.
        //We check here if Position is a range before calling .end on it.
        //This is not needed for scala 2.11.x.
        if (qualifier.pos.isRange) {
          handleTypeCompletion(qualifier.pos.end, name.decoded, dotOffset)
        } else {
          //not prefixed
          (0, Seq.empty)
        }

      case t @ pressy.Import(expr, selectors)  =>
        // If the selectors haven't been defined yet...
        if (selectors.head.name.toString == "<error>") {
          if (expr.tpe.toString == "<error>") {
              // If the expr is badly typed, try to scope complete it
              if (expr.isInstanceOf[pressy.Ident]) {
                val exprName =  expr.asInstanceOf[pressy.Ident].name.decoded
                val pos =
                  // Without the first case, things like `import <caret>` are
                  // returned a wrong position.
                  if (exprName == "<error>") expr.pos.point - 1
                  else expr.pos.point
                pos -> handleCompletion(
                  ask(expr.pos.point, pressy.askScopeCompletion),
                  // if it doesn't have a name at all, accept anything
                  if (exprName == "<error>") "" else exprName
                )
              } else (expr.pos.point, Seq.empty)
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

        val res =
          if (shallow.length > 0) shallow
          else if (deep.length == 1) deep
          else deep :+ ("" -> None)

        (t.pos.start, res)

      case t =>
        val comps = ask(index, pressy.askScopeCompletion)

        index -> pressy.ask(() =>
          comps.filter(m => !blacklisted(m.sym))
               .map { s => (memberToString(s), None) }
        )
    }
    def ask(index: Int, query: (Position, Response[List[pressy.Member]]) => Unit) = {
      val position = new OffsetPosition(currentFile, index)
      //if a match can't be found awaitResponse throws an Exception.
      val result = Try(Compiler.awaitResponse[List[pressy.Member]](query(position, _)))
      result match {
        case Success(scopes) => scopes.filter(_.accessible)
        case Failure(error) => List.empty[pressy.Member]
      }
    }

  }
  def apply(classpath: Seq[java.io.File],
            dynamicClasspath: VirtualDirectory,
            evalClassloader: => ClassLoader,
            settings: Settings): Pressy = new Pressy {

    var cachedPressy: nsc.interactive.Global = null

    def initPressy = {
      val (dirDeps, jarDeps) = classpath.partition(_.isDirectory)
      val jcp = GlobalInitCompat.initGlobalClasspath(
        dirDeps, jarDeps,
        dynamicClasspath,
        settings
      )
      val reporter = makeReporter(_ => (), _ => (), _ => (), settings)
      GlobalInitCompat.initInteractiveGlobal(settings, reporter, jcp, evalClassloader)
    }

    def run(snippetIndex: Int, previousImports: String, snippet: String) = {
      val prefix = previousImports + newLine + "object AutocompleteWrapper{" + newLine
      val suffix = newLine + "}"
      val allCode = prefix + snippet + suffix
      val index = snippetIndex + prefix.length
      if (cachedPressy == null) cachedPressy = initPressy

      val pressy = cachedPressy
      val currentFile = new BatchSourceFile(
        Compiler.makeFile(allCode.getBytes, name = "Current.sc"),
        allCode)

      val r = new Response[Unit]
      pressy.askReload(List(currentFile), r)
      r.get.fold(x => x, e => throw e)

      (prefix, Try(new Run(pressy, currentFile, allCode, index)))
    }

    def complete(snippetIndex: Int, previousImports: String, snippet: String) = {

      val (prefix, run0) = run(snippetIndex, previousImports, snippet)

      val (i, all): (Int, Seq[(String, Option[String])]) = run0.map(_.prefixed) match {
        case Success(prefixed) => prefixed
        case Failure(throwable) => (0, Seq.empty)
      }

      val allNames = all.collect { case (name, None) => name }.sorted.distinct

      val signatures = all.collect { case (name, Some(defn)) => defn }.sorted.distinct

      (i - prefix.length, allNames, signatures)
    }

    def info(snippetIndex: Int, previousImports: String, snippet: String): Try[String] = {
      val (_, run0) = run(snippetIndex, previousImports, snippet)
      run0.map(_.tree.symbol.info.toString)
    }

    def shutdownPressy() = {
      Option(cachedPressy).foreach(_.askShutdown())
      cachedPressy = null
    }
  }
}
