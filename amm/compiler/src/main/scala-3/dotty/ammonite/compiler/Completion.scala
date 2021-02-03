package dotty.ammonite.compiler

// Originally adapted from
// https://github.com/lampepfl/dotty/blob/3.0.0-M1/
//   compiler/src/dotty/tools/dotc/interactive/Completion.scala
// Then tweaked for deep completion, import $ivy completion, …

import java.nio.charset.Charset

import dotty.tools.dotc
import dotc.ast.Trees._
import dotc.ast.untpd
import dotc.config.Printers.interactiv
import dotc.core.Contexts._
import dotc.core.CheckRealizable
import dotc.core.Decorators._
import dotc.core.Denotations.SingleDenotation
import dotc.core.Flags._
import dotc.core.Names.{Name, TermName, termName}
import dotc.core.NameKinds.SimpleNameKind
import dotc.core.NameOps._
import dotc.core.Symbols.{NoSymbol, Symbol, defn}
import dotc.core.Scopes
import dotc.core.StdNames.{nme, tpnme}
import dotc.core.TypeError
import dotc.core.Types.{NameFilter, NamedType, NoType, Type}
import dotc.interactive._
import dotc.util.Chars.{isOperatorPart, isScalaLetter}
import dotc.util.{NameTransformer, NoSourcePosition, SourcePosition}

import scala.collection.mutable

/**
 * One of the results of a completion query.
 *
 * @param label         The label of this completion result, or the text that this completion result
 *                      should insert in the scope where the completion request happened.
 * @param description   The description of this completion result: the fully qualified name for
 *                      types, or the type for terms.
 * @param symbols       The symbols that are matched by this completion result.
 */
case class Completion(name: Name, description: String, symbols: List[Symbol]) {
  def label: String = {

    // adapted from
    // https://github.com/scala/scala/blob/decbd53f1bde4600c8ff860f30a79f028a8e431d/
    //   src/reflect/scala/reflect/internal/Printers.scala#L573-L584
    val bslash = '\\'
    val isDot = (x: Char) => x == '.'
    val brackets = List('[',']','(',')','{','}')

    def quotedName(name: Name): String = {
      val s = name.decode
      val term = name.toTermName
      if (nme.keywords(term) && term != nme.USCOREkw) s"`$s`"
      else s.toString
    }

    val decName = name.decode.toString
    def addBackquotes(s: String) = {
      val hasSpecialChar = decName.exists { ch =>
        brackets.contains(ch) || ch.isWhitespace || isDot(ch)
      }
      def isOperatorLike = (name.isOperatorName || decName.exists(isOperatorPart)) &&
        decName.exists(isScalaLetter) &&
        !decName.contains(bslash)
      if (hasSpecialChar || isOperatorLike) s"`$s`"
      else s
    }

    if (name == nme.CONSTRUCTOR) "this"
    else addBackquotes(quotedName(name))
  }
}

object Completion {

  import dotc.ast.tpd._

  /** Get possible completions from tree at `pos`
   *
   *  @return offset and list of symbols for possible completions
   */
  def completions(
    pos: SourcePosition,
    dependencyCompleteOpt: Option[String => (Int, Seq[String])]
  )(using Context): (Int, List[Completion]) = {
    val path = Interactive.pathTo(ctx.compilationUnit.tpdTree, pos.span)
    computeCompletions(pos, path, dependencyCompleteOpt)(using Interactive.contextOfPath(path))
  }

  /**
   * Inspect `path` to determine what kinds of symbols should be considered.
   *
   * If the path starts with:
   *  - a `RefTree`, then accept symbols of the same kind as its name;
   *  - a renaming import, and the cursor is on the renamee, accept both terms and types;
   *  - an import, accept both terms and types;
   *
   * Otherwise, provide no completion suggestion.
   */
  private def completionMode(path: List[Tree], pos: SourcePosition): Mode =
    path match {
      case (ref: RefTree) :: _ =>
        if (ref.name.isTermName) Mode.Term
        else if (ref.name.isTypeName) Mode.Type
        else Mode.None

      case (sel: untpd.ImportSelector) :: _ =>
        if sel.imported.span.contains(pos.span) then Mode.Import
        else Mode.None // Can't help completing the renaming

      case Import(_, _) :: _ =>
        Mode.Import

      case _ =>
        Mode.None
    }

  /**
   * Inspect `path` to determine the completion prefix. Only symbols whose name start with the
   * returned prefix should be considered.
   */
  private def completionPrefix(path: List[untpd.Tree], pos: SourcePosition): String =
    path match {
      case (sel: untpd.ImportSelector) :: _ =>
        completionPrefix(sel.imported :: Nil, pos)

      case Import(expr, selectors) :: _ =>
        selectors.find(_.span.contains(pos.span)).map { selector =>
          completionPrefix(selector :: Nil, pos)
        }.getOrElse("")

      case (ref: untpd.RefTree) :: _ =>
        if (ref.name == nme.ERROR) ""
        else ref.name.toString.take(pos.span.point - ref.span.point)

      case _ =>
        ""
    }

  /** Inspect `path` to determine the offset where the completion result should be inserted. */
  private def completionOffset(path: List[Tree]): Int =
    path match {
      case (ref: RefTree) :: _ => ref.span.point
      case _ => 0
    }

  /** Create a new `CompletionBuffer` for completing at `pos`. */
  private def completionBuffer(path: List[Tree], pos: SourcePosition): CompletionBuffer = {
    val mode = completionMode(path, pos)
    val prefix = completionPrefix(path, pos)
    new CompletionBuffer(mode, prefix, pos)
  }

  private def computeCompletions(
    pos: SourcePosition,
    path: List[Tree],
    dependencyCompleteOpt: Option[String => (Int, Seq[String])]
  )(using Context): (Int, List[Completion]) = {

    val offset = completionOffset(path)
    val buffer = completionBuffer(path, pos)

    var extra = List.empty[Completion]

    if (buffer.mode != Mode.None)
      path match {
        case Select(qual, _) :: _                              => buffer.addMemberCompletions(qual)
        case Import(Ident(name), _) :: _ if name.decode.toString == "$ivy" =>
          dependencyCompleteOpt match {
            case None => 0 -> Seq.empty[(String, Option[String])]
            case Some(complete) =>
              val input = buffer.prefix
              val (pos, completions) = complete(input)
              val input0 = input.take(pos)
              extra = completions.distinct.toList
                .map(s => Completion(termName(input0 + s), "", Nil))
          }
        case Import(expr, _) :: _                              => buffer.addMemberCompletions(expr)
        // (Dotty comment) TODO: distinguish given from plain imports
        case (_: untpd.ImportSelector) :: Import(expr, _) :: _ => buffer.addMemberCompletions(expr)
        case _                                                 =>
          buffer.addScopeCompletions
          // Too slow for now
          // if (buffer.getCompletions.isEmpty)
          //   buffer.addDeepCompletions
      }

    val completionList = extra ++ buffer.getCompletions

    interactiv.println(i"""completion with pos     = $pos,
                          |                offset  = ${offset},
                          |                prefix  = ${buffer.prefix},
                          |                term    = ${buffer.mode.is(Mode.Term)},
                          |                type    = ${buffer.mode.is(Mode.Type)}
                          |                results = $completionList%, %""")
    (pos.span.start - buffer.prefix.length, completionList)
  }

  private class CompletionBuffer(val mode: Mode, val prefix: String, pos: SourcePosition) {

    private val completions = new RenameAwareScope

    /**
     * Return the list of symbols that should be included in completion results.
     *
     * If several symbols share the same name, the type symbols appear before term symbols inside
     * the same `Completion`.
     */
    def getCompletions(using Context): List[Completion] = {
      val nameToSymbols = completions.mappings.toList
      nameToSymbols.map { case (name, symbols) =>
        val typesFirst = symbols.sortWith((s1, s2) => s1.isType && !s2.isType)
        val desc = description(typesFirst)
        // kind of meh, not sure how to make that more reliable in Scala 3
        Completion(name, desc, typesFirst)
      }
    }

    /**
     * A description for completion result that represents `symbols`.
     *
     * If `symbols` contains a single symbol, show its full name in case it's a type, or its type if
     * it's a term.
     *
     * When there are multiple symbols, show their kinds.
     */
    private def description(symbols: List[Symbol])(using Context): String =
      symbols match {
        case sym :: Nil =>
          if (sym.isType) sym.showFullName
          else sym.info.widenTermRefExpr.show

        case sym :: _ =>
          symbols.map(ctx.printer.kindString).mkString("", " and ", s" ${sym.name.show}")

        case Nil =>
          ""
      }

    /**
     * Add symbols that are currently in scope to `info`: the members of the current class and the
     * symbols that have been imported.
     */
    def addScopeCompletions(using Context): Unit = {
      if (ctx.owner.isClass) {
        addAccessibleMembers(ctx.owner.thisType)
        ctx.owner.asClass.classInfo.selfInfo match {
          case selfSym: Symbol => add(selfSym, selfSym.name)
          case _ =>
        }
      }
      else if (ctx.scope != null) ctx.scope.foreach(s => add(s, s.name))

      addImportCompletions

      var outer = ctx.outer
      while ((outer.owner `eq` ctx.owner) && (outer.scope `eq` ctx.scope)) {
        addImportCompletions(using outer)
        outer = outer.outer
      }
      if (outer `ne` NoContext) addScopeCompletions(using outer)
    }

    /**
     * Find all the members of `qual` and add the ones that pass the include filters to `info`.
     *
     * If `info.mode` is `Import`, the members added via implicit conversion on `qual` are not
     * considered.
     */
    def addMemberCompletions(qual: Tree)(using Context): Unit =
      if (!qual.tpe.widenDealias.isExactlyNothing) {
        addAccessibleMembers(qual.tpe)
        if (!mode.is(Mode.Import) && !qual.tpe.isNullType)
          // Implicit conversions do not kick in when importing
          // and for `NullClass` they produce unapplicable completions (for unclear reasons)
          implicitConversionTargets(qual)(using ctx.fresh.setExploreTyperState())
            .foreach(addAccessibleMembers)
      }

    /**
     * If `sym` exists, no symbol with the same name is already included, and it satisfies the
     * inclusion filter, then add it to the completions.
     */
    private def add(sym: Symbol, nameInScope: Name, deep: Boolean = false)(using Context) =
      if (sym.exists &&
          (deep || completionsFilter(NoType, nameInScope)) &&
          !completions.lookup(nameInScope).exists &&
          include(sym, nameInScope, deep))
        completions.enter(sym, nameInScope)

    /** Lookup members `name` from `site`, and try to add them to the completion list. */
    private def addMember(site: Type, name: Name, nameInScope: Name)(using Context) =
      if (!completions.lookup(nameInScope).exists)
        for (alt <- site.member(name).alternatives) add(alt.symbol, nameInScope)

    /** Include in completion sets only symbols that
     *   1. start with given name prefix, and
     *   2. is not absent (info is not NoType)
     *   3. are not a primary constructor,
     *   4. have an existing source symbol,
     *   5. are the module class in case of packages,
     *   6. are mutable accessors, to exclude setters for `var`,
     *   7. symbol is not a package object
     *   8. symbol is not an artifact of the compiler
     *   9. have same term/type kind as name prefix given so far
     */
    private def include(
      sym: Symbol,
      nameInScope: Name,
      deep: Boolean = false
    )(using Context): Boolean =
      (deep || nameInScope.startsWith(prefix)) &&
      !sym.isAbsent() &&
      !sym.isPrimaryConstructor &&
      sym.sourceSymbol.exists &&
      (!sym.is(Package) || sym.is(ModuleClass)) &&
      !sym.isAllOf(Mutable | Accessor) &&
      !sym.isPackageObject &&
      !sym.is(Artifact) &&
      (
           (mode.is(Mode.Term) && sym.isTerm)
        || (mode.is(Mode.Type) && (sym.isType || sym.isStableMember))
      )

    /**
     * Find all the members of `site` that are accessible and which should be included in `info`.
     *
     * @param site The type to inspect.
     * @return The members of `site` that are accessible and pass the include filter of `info`.
     */
    private def accessibleMembers(site: Type)(using Context): Seq[Symbol] = site match {
      case site: NamedType if site.symbol.is(Package) =>
        extension (tpe: Type)
          def accessibleSymbols = tpe
            .decls
            .toList
            .filter(sym => sym.isAccessibleFrom(site, superAccess = false))

        val packageDecls = site.accessibleSymbols
        val packageObjectsDecls = packageDecls
          .filter(_.isPackageObject)
          .flatMap(_.thisType.accessibleSymbols)
        packageDecls ++ packageObjectsDecls
      case _ =>
        def appendMemberSyms(name: Name, buf: mutable.Buffer[SingleDenotation]): Unit =
          try buf ++= site.member(name).alternatives
          catch { case ex: TypeError => }
        site.memberDenots(completionsFilter, appendMemberSyms).collect {
          case mbr if include(mbr.symbol, mbr.symbol.name) =>
            mbr.accessibleFrom(site, superAccess = true).symbol
          case _ => NoSymbol
        }.filter(_.exists)
    }

    /** Add all the accessible members of `site` in `info`. */
    private def addAccessibleMembers(site: Type)(using Context): Unit =
      for (mbr <- accessibleMembers(site)) addMember(site, mbr.name, mbr.name)

    /**
     * Add in `info` the symbols that are imported by `ctx.importInfo`. If this is a wildcard
     * import, all the accessible members of the import's `site` are included.
     */
    private def addImportCompletions(using Context): Unit = {
      val imp = ctx.importInfo
      if (imp != null) {
        def addImport(name: TermName, nameInScope: TermName) = {
          addMember(imp.site, name, nameInScope)
          addMember(imp.site, name.toTypeName, nameInScope.toTypeName)
        }
        imp.reverseMapping.foreachBinding { (nameInScope, original) =>
          if (original != nameInScope || !imp.excluded.contains(original))
            addImport(original, nameInScope)
        }
        if (imp.isWildcardImport)
          for (mbr <- accessibleMembers(imp.site) if !imp.excluded.contains(mbr.name.toTermName))
            addMember(imp.site, mbr.name, mbr.name)
      }
    }
    private def blacklisted(s: Symbol)(using Context) = {
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
        "java.lang.Object.notify",
        "java.lang.Object.clone",
        "java.lang.Object.finalize"
      )

      blacklist(s.showFullName) ||
      s.isOneOf(GivenOrImplicit) ||
      // Cache objects, which you should probably never need to
      // access directly, and apart from that have annoyingly long names
      "cache[a-f0-9]{32}".r.findPrefixMatchOf(s.name.decode.toString).isDefined ||
      // s.isDeprecated ||
      s.name.decode.toString == "<init>" ||
      s.name.decode.toString.contains('$')
    }
    def addDeepCompletions(using Context): Unit = {

      val blacklistedPackages = Set("shaded")

      def allMembers(s: Symbol) =
        try s.info.allMembers
        catch {
          case _: dotc.core.TypeError => Nil
        }
      def rec(t: Symbol): Seq[Symbol] = {
        if (blacklistedPackages(t.name.toString))
          Nil
        else {
          val fullName = t.fullName.toString
          val children =
            if (t.is(Package) || t.is(PackageVal) || t.is(PackageClass)) {
              allMembers(t).map(_.symbol).filter(!blacklisted(_)).filter(_ != t).flatMap(rec)
            } else Nil

          t +: children.toSeq
        }
      }

      for {
        member <- allMembers(defn.RootClass).map(_.symbol).filter(!blacklisted(_)).toList
        sym <- rec(member)
        if sym.name.toString.startsWith(prefix)
      } add(sym, sym.fullName, deep = true)
    }

    /**
     * Given `qual` of type T, finds all the types S such that there exists an implicit conversion
     * from T to S.
     *
     * @param qual The argument to which the implicit conversion should be applied.
     * @return The set of types that `qual` can be converted to.
     */
    private def implicitConversionTargets(qual: Tree)(using Context): Set[Type] = {
      val typer = ctx.typer
      val conversions = new typer.ImplicitSearch(defn.AnyType, qual, pos.span).allImplicits
      val targets = conversions.map(_.widen.finalResultType)
      interactiv.println(i"implicit conversion targets considered: ${targets.toList}%, %")
      targets
    }

    /** Filter for names that should appear when looking for completions. */
    private object completionsFilter extends NameFilter {
      def apply(pre: Type, name: Name)(using Context): Boolean =
        !name.isConstructorName && name.toTermName.info.kind == SimpleNameKind
      def isStable = true
    }
  }

  /**
   * The completion mode: defines what kinds of symbols should be included in the completion
   * results.
   */
  private class Mode(val bits: Int) extends AnyVal {
    def is(other: Mode): Boolean = (bits & other.bits) == other.bits
    def |(other: Mode): Mode = new Mode(bits | other.bits)
  }
  private object Mode {
    /** No symbol should be included */
    val None: Mode = new Mode(0)

    /** Term symbols are allowed */
    val Term: Mode = new Mode(1)

    /** Type and stable term symbols are allowed */
    val Type: Mode = new Mode(2)

    /** Both term and type symbols are allowed */
    val Import: Mode = new Mode(4) | Term | Type
  }

  /** A scope that tracks renames of the entered symbols.
   *  Useful for providing completions for renamed symbols
   *  in the REPL and the IDE.
   */
  private class RenameAwareScope extends Scopes.MutableScope {
    private val nameToSymbols: mutable.Map[TermName, List[Symbol]] = mutable.Map.empty

    /** Enter the symbol `sym` in this scope, recording a potential renaming. */
    def enter[T <: Symbol](sym: T, name: Name)(using Context): T = {
      val termName = name.stripModuleClassSuffix.toTermName
      nameToSymbols += termName -> (sym :: nameToSymbols.getOrElse(termName, Nil))
      newScopeEntry(name, sym)
      sym
    }

    /** Get the names that are known in this scope, along with the list of symbols they refer to. */
    def mappings: Map[TermName, List[Symbol]] = nameToSymbols.toMap
  }
}

