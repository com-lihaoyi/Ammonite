package dotty.ammonite.compiler

// TODO Remove this file once lampepfl/dotty#11794 is merged

import java.nio.charset.Charset

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.config.Printers.interactiv
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.CheckRealizable
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names.{Name, TermName}
import dotty.tools.dotc.core.NameKinds.SimpleNameKind
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol, TermSymbol, defn, newSymbol}
import dotty.tools.dotc.core.Scopes.Scope
import dotty.tools.dotc.core.StdNames.{nme, tpnme}
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.TypeComparer
import dotty.tools.dotc.core.TypeError
import dotty.tools.dotc.core.Types.{
  ExprType, MethodOrPoly, NameFilter, NamedType, NoType, PolyType, TermRef, Type
}
import dotty.tools.dotc.interactive.Interactive
import dotty.tools.dotc.printing.Texts._
import dotty.tools.dotc.util.Chars.{isOperatorPart, isScalaLetter}
import dotty.tools.dotc.util.{NameTransformer, NoSourcePosition, SourcePosition}

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
case class Completion(label: String, description: String, symbols: List[Symbol])

object Completion {

  import dotty.tools.dotc.ast.tpd._

  /** Get possible completions from tree at `pos`
   *
   *  @return offset and list of symbols for possible completions
   */
  def completions(pos: SourcePosition)(using Context): (Int, List[Completion]) = {
    val path = Interactive.pathTo(ctx.compilationUnit.tpdTree, pos.span)
    computeCompletions(pos, path)(using Interactive.contextOfPath(path))
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
  def completionMode(path: List[Tree], pos: SourcePosition): Mode =
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
  def completionPrefix(path: List[untpd.Tree], pos: SourcePosition): String =
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
  def completionOffset(path: List[Tree]): Int =
    path match {
      case (ref: RefTree) :: _ => ref.span.point
      case _ => 0
    }

  private def computeCompletions(
    pos: SourcePosition,
    path: List[Tree]
  )(using Context): (Int, List[Completion]) = {
    val mode = completionMode(path, pos)
    val prefix = completionPrefix(path, pos)
    val completer = new Completer(mode, prefix, pos)

    val completions = path match {
        case Select(qual, _) :: _                              =>
          completer.selectionCompletions(qual)
        case Import(expr, _) :: _                              =>
          completer.directMemberCompletions(expr)
        case (_: untpd.ImportSelector) :: Import(expr, _) :: _ =>
          completer.directMemberCompletions(expr)
        case _                                                 =>
          completer.scopeCompletions
      }

    val describedCompletions = describeCompletions(completions)
    val offset = completionOffset(path)

    interactiv.println(i"""completion with pos     = $pos,
                          |                prefix  = ${completer.prefix},
                          |                term    = ${completer.mode.is(Mode.Term)},
                          |                type    = ${completer.mode.is(Mode.Type)}
                          |                results = $describedCompletions%, %""")
    (offset, describedCompletions)
  }

  /**
   * Return the list of code completions with descriptions based on a mapping from names to the
   * denotations they refer to. If several denotations share the same name, the type denotations
   * appear before term denotations inside the same `Completion`.
   */
  def describeCompletions(completions: CompletionMap)(using Context): List[Completion] = {
    completions
      .toList.groupBy(_._1.toTermName) // don't distinguish between names of terms and types
      .toList.map { (name, namedDenots) =>
        val denots = namedDenots.flatMap(_._2)
        val typesFirst = denots.sortWith((d1, d2) => d1.isType && !d2.isType)
        val desc = description(typesFirst)
        Completion(label(name), desc, typesFirst.map(_.symbol))
    }
  }

  /**
   * A description for completion result that represents `symbols`.
   *
   * If `denots` contains a single denotation, show its full name in case it's a type, or its type
   * if it's a term.
   *
   * When there are multiple denotations, show their kinds.
   */
  private def description(denots: List[SingleDenotation])(using Context): String =
    denots match {
      case denot :: Nil =>
        if (denot.isType) denot.symbol.showFullName
        else denot.info.widenTermRefExpr.show

      case denot :: _ =>
        denots
          .map(den => ctx.printer.kindString(den.symbol))
          .distinct
          .mkString("", " and ", s" ${denot.name.show}")

      case Nil =>
        ""
    }

  /** Computes code completions depending on the context in which completion is requested
   *  @param mode    Should complete names of terms, types or both
   *  @param prefix  The prefix that all suggested completions should start with
   *  @param pos     Cursor position where completion was requested
   *
   *  For the results of all `xyzCompletions` methods term names and type names are always treated
   *  as different keys in the same map and they never conflict with each other.
   */
  class Completer(val mode: Mode, val prefix: String, pos: SourcePosition) {
    /** Completions for terms and types that are currently in scope:
     *  the members of the current class, local definitions and the symbols that have been imported,
     *  recursively adding completions from outer scopes.
     *  In case a name is ambiguous, no completions are returned for it.
     *  This mimics the logic for deciding what is ambiguous used by the compiler.
     *  In general in case of a name clash symbols introduced in more deeply nested scopes
     *  have higher priority and shadow previous definitions with the same name although:
     *  - imports with the same level of nesting cause an ambiguity
     *  - members and local definitions with the same level of nesting are allowed for overloading
     *  - an import is ignored if there is a local definition or a member introduced in the same
     *    scope (even if the import follows it syntactically)
     *  - a more deeply nested import shadowing a member or a local definition causes an ambiguity
     */
    def scopeCompletions(using context: Context): CompletionMap = {
      val mappings =
        collection.mutable.Map.empty[Name, List[ScopedDenotations]]
          .withDefaultValue(List.empty)
      def addMapping(name: Name, denots: ScopedDenotations) =
        mappings(name) = mappings(name) :+ denots
      var ctx = context

      while ctx ne NoContext do
        if ctx.isImportContext then
          importedCompletions(using ctx).foreach { (name, denots) =>
            addMapping(name, ScopedDenotations(denots, ctx))
          }
        else if ctx.owner.isClass then
          accessibleMembers(ctx.owner.thisType)
            .groupByName.foreach { (name, denots) =>
              addMapping(name, ScopedDenotations(denots, ctx))
            }
        else if ctx.scope != null then
          ctx.scope.toList.filter(symbol => include(symbol, symbol.name))
            .flatMap(_.alternatives)
            .groupByName.foreach { (name, denots) =>
              addMapping(name, ScopedDenotations(denots, ctx))
            }

        ctx = ctx.outer
      end while

      var resultMappings = Map.empty[Name, Seq[SingleDenotation]]

      mappings.foreach { (name, denotss) =>
        val first = denotss.head
        denotss.find(!_.ctx.isImportContext) match {
          // most deeply nested member or local definition if not shadowed by an import
          case Some(local) if local.ctx.scope == first.ctx.scope =>
            resultMappings += name -> local.denots

          // most deeply nested import if not shadowed by another import
          case None if denotss.length < 2 || (denotss(1).ctx.scope ne first.ctx.scope) =>
            resultMappings += name -> first.denots

          case _ =>
        }
      }

      resultMappings
    }

    /** Completions for selections from a term.
     *  Direct members take priority over members from extensions
     *  and so do members from extensions over members from implicit conversions
     */
    def selectionCompletions(qual: Tree)(using Context): CompletionMap =
      implicitConversionMemberCompletions(qual) ++
        extensionCompletions(qual) ++
        directMemberCompletions(qual)

    /** Completions for members of `qual`'s type.
     *  These include inherited definitions but not members added by extensions or implicit
     *  conversions
     */
    def directMemberCompletions(qual: Tree)(using Context): CompletionMap =
      if qual.tpe.widenDealias.isExactlyNothing then
        Map.empty
      else
        accessibleMembers(qual.tpe).groupByName

    /** Completions introduced by imports directly in this context.
     *  Completions from outer contexts are not included.
     */
    private def importedCompletions(using Context): CompletionMap = {
      val imp = ctx.importInfo

      def fromImport(name: Name, nameInScope: Name): Seq[(Name, SingleDenotation)] =
        imp.site.member(name).alternatives
          .collect { case denot if include(denot, nameInScope) => nameInScope -> denot }

      if imp == null then
        Map.empty
      else
        val givenImports = imp.importedImplicits
          .map { ref => (ref.implicitName: Name, ref.underlyingRef.denot.asSingleDenotation) }
          .filter((name, denot) => include(denot, name))
          .groupByName

        val wildcardMembers =
          if imp.selectors.exists(_.imported.name == nme.WILDCARD) then
            val denots = accessibleMembers(imp.site)
              .filter(mbr => !mbr.symbol.is(Given) && !imp.excluded.contains(mbr.name.toTermName))
            denots.groupByName
          else
            Map.empty

        val explicitMembers =
          val importNamesInScope = imp.forwardMapping.toList.map(_._2)
          val duplicatedNames = importNamesInScope.diff(importNamesInScope.distinct)
          val discardedNames = duplicatedNames ++ imp.excluded
          imp.reverseMapping.toList
            .filter { (nameInScope, _) => !discardedNames.contains(nameInScope) }
            .flatMap { (nameInScope, original) =>
              fromImport(original, nameInScope) ++
              fromImport(original.toTypeName, nameInScope.toTypeName)
            }.toSeq.groupByName

        givenImports ++ wildcardMembers ++ explicitMembers
    }

    /** Completions from implicit conversions including old style extensions using implicit
     *  classes */
    private def implicitConversionMemberCompletions(qual: Tree)(using Context): CompletionMap =
      if qual.tpe.widenDealias.isExactlyNothing || qual.tpe.isNullType then
        Map.empty
      else
        val membersFromConversion =
          implicitConversionTargets(qual)(using ctx.fresh.setExploreTyperState())
            .flatMap(accessibleMembers)
        membersFromConversion.toSeq.groupByName

    /** Completions from extension methods */
    private def extensionCompletions(qual: Tree)(using Context): CompletionMap =
      def asDefLikeType(tpe: Type): Type = tpe match
        case _: MethodOrPoly => tpe
        case _ => ExprType(tpe)

      def tryApplyingReceiverToExtension(termRef: TermRef): Option[SingleDenotation] =
        None

      def extractMemberExtensionMethods(types: Seq[Type]): Seq[(TermRef, TermName)] =
        object DenotWithMatchingName:
          def unapply(denot: SingleDenotation): Option[(SingleDenotation, TermName)] =
            denot.name match
              case name: TermName if include(denot, name) => Some((denot, name))
              case _ => None

        types.flatMap { tpe =>
          tpe.membersBasedOnFlags(required = ExtensionMethod, excluded = EmptyFlags)
            .collect {
              case DenotWithMatchingName(denot, name) =>
                TermRef(tpe, denot.symbol) -> name
            }
        }

      // There are four possible ways for an extension method to be applicable

      // 1. The extension method is visible under a simple name, by being defined or inherited or
      //    imported in a scope enclosing the reference.
      val termCompleter = new Completer(Mode.Term, prefix, pos)
      val extMethodsInScope = termCompleter.scopeCompletions.toList.flatMap {
        case (name, denots) =>
          denots.collect { case d: SymDenotation => (d.termRef, name.asTermName) }
      }

      // 2. The extension method is a member of some given instance that is visible at the point
      //    of the reference.
      val givensInScope = ctx.implicits.eligible(defn.AnyType).map(_.implicitRef.underlyingRef)
      val extMethodsFromGivensInScope = extractMemberExtensionMethods(givensInScope)

      // 3. The reference is of the form r.m and the extension method is defined in the implicit
      //    scope of the type of r.
      val implicitScopeCompanions = ctx.run.implicitScope(qual.tpe).companionRefs.showAsList
      val extMethodsFromImplicitScope = extractMemberExtensionMethods(implicitScopeCompanions)

      // 4. The reference is of the form r.m and the extension method is defined in some given
      //    instance in the implicit scope of the type of r.
      val givensInImplicitScope =
        implicitScopeCompanions
          .flatMap(_.membersBasedOnFlags(required = Given, excluded = EmptyFlags))
          .map(_.info)
      val extMethodsFromGivensInImplicitScope = extractMemberExtensionMethods(givensInImplicitScope)

      val availableExtMethods =
        extMethodsFromGivensInImplicitScope ++
          extMethodsFromImplicitScope ++
          extMethodsFromGivensInScope ++
          extMethodsInScope
      val extMethodsWithAppliedReceiver = availableExtMethods.flatMap {
        case (termRef, termName) =>
          if termRef.symbol.is(ExtensionMethod) && !qual.tpe.isBottomType then
            tryApplyingReceiverToExtension(termRef)
              .map(denot => termName -> denot)
          else None
      }
      extMethodsWithAppliedReceiver.groupByName

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
    private def include(denot: SingleDenotation, nameInScope: Name)(using Context): Boolean =
      val sym = denot.symbol

      nameInScope.startsWith(prefix) &&
      sym.exists &&
      completionsFilter(NoType, nameInScope) &&
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

    /** @param site The type to inspect.
     *  @return The members of `site` that are accessible and pass the include filter.
     */
    private def accessibleMembers(site: Type)(using Context): Seq[SingleDenotation] = {
      def appendMemberSyms(name: Name, buf: mutable.Buffer[SingleDenotation]): Unit =
        try
          buf ++= site.member(name).alternatives
        catch
          case ex: TypeError =>

      site.memberDenots(completionsFilter, appendMemberSyms).collect {
        case mbr if include(mbr, mbr.name)
                    && mbr.symbol.isAccessibleFrom(site) => mbr
      }
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

    extension (denotations: Seq[SingleDenotation])
      def groupByName(using Context): CompletionMap = denotations.groupBy(_.name)

    extension [N <: Name](namedDenotations: Seq[(N, SingleDenotation)])
      @annotation.targetName("groupByNameTupled")
      def groupByName: CompletionMap =
        namedDenotations
          .groupMap((name, denot) => name)((name, denot) => denot)
  }

  private type CompletionMap = Map[Name, Seq[SingleDenotation]]

  /** Temporary data structure representing denotations with the same name introduced in a given
   *  scope as a member of a type, by a local definition or by an import clause
   */
  private case class ScopedDenotations(denots: Seq[SingleDenotation], ctx: Context)

  /**
   * The completion mode: defines what kinds of symbols should be included in the completion
   * results.
   */
  class Mode(val bits: Int) extends AnyVal {
    def is(other: Mode): Boolean = (bits & other.bits) == other.bits
    def |(other: Mode): Mode = new Mode(bits | other.bits)
  }
  object Mode {
    /** No symbol should be included */
    val None: Mode = new Mode(0)

    /** Term symbols are allowed */
    val Term: Mode = new Mode(1)

    /** Type and stable term symbols are allowed */
    val Type: Mode = new Mode(2)

    /** Both term and type symbols are allowed */
    val Import: Mode = new Mode(4) | Term | Type
  }

  private val bslash = '\\'
  private val isDot = (x: Char) => x == '.'
  private val brackets = List('[',']','(',')','{','}')

  def label(name: Name): String = {

    def maybeQuote(name: Name, recurse: Boolean): String =
      if (recurse && name.isTermName)
        name.asTermName.qualToString(maybeQuote(_, true), maybeQuote(_, false))
      // initially adapted from
      // https://github.com/scala/scala/blob/decbd53f1bde4600c8ff860f30a79f028a8e431d/
      //   src/reflect/scala/reflect/internal/Printers.scala#L573-L584
      else if (name == nme.CONSTRUCTOR) "this"
      else {
        val decName = name.decode.toString
        val hasSpecialChar = decName.exists { ch =>
          brackets.contains(ch) || ch.isWhitespace || isDot(ch)
        }
        def isOperatorLike = (name.isOperatorName || decName.exists(isOperatorPart)) &&
          decName.exists(isScalaLetter) &&
          !decName.contains(bslash)
        lazy val term = name.toTermName

        val needsBackTicks = hasSpecialChar ||
          isOperatorLike ||
          nme.keywords(term) && term != nme.USCOREkw

        if (needsBackTicks) s"`$decName`"
        else decName
      }

    maybeQuote(name, true)
  }
}

