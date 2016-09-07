/**
  * Various common, "dumb" data-structures that represent common things that
  * are passed around inside Ammonite
  */
package ammonite.util

import pprint.{PPrint, PPrinter}

import scala.collection.mutable
import scala.reflect.NameTransformer
import scala.reflect.runtime.universe.TypeTag

/**
  * Exception for reporting script compilation failures
  */
class CompilationError(message: String) extends Exception(message)

case class Evaluated(wrapper: Seq[Name], imports: Imports, tag: String)

/**
  * Represents the importing of a single name in the Ammonite REPL, of the
  * form
  *
  * {{{
  * import $prefix.{$fromName => $toName}
  * }}}
  *
  * All imports are reduced to this form; `import $prefix.$name` is results in
  * the `fromName` and `toName` being the same, while `import $prefix._` or
  * `import $prefix.{foo, bar, baz}` are split into multiple distinct
  * [[ImportData]] objects.
  *
  * Note that imports can be of one of three distinct `ImportType`s: importing
  * a type, a term, or both. This lets us properly deal with shadowing correctly
  * if we import the type and term of the same name from different places
  */
case class ImportData(fromName: Name,
                      toName: Name,
                      prefix: Seq[Name],
                      importType: ImportData.ImportType)

object ImportData {
  sealed case class ImportType(name: String)
  val Type = ImportType("Type")
  val Term = ImportType("Term")
  val TermType = ImportType("TermType")
}

/**
  * Represents the imports that occur before a piece of user code in the
  * Ammonite REPL. It's basically a `Seq[ImportData]`, except we really want
  * it to be always in a "canonical" form without shadowed/duplicate imports.
  *
  * Thus we only expose an `apply` method which performs this de-duplication,
  * and a `++` operator that combines two sets of imports while performing
  * de-duplication.
  */
class Imports private (val value: Seq[ImportData]) {
  def ++(others: Imports) = Imports(this.value, others.value)
  override def toString() = s"Imports(${value.toString})"
}

object Imports {
  // This isn't called directly, but we need to define it so uPickle can know
  // how to read/write imports
  def unapply(s: Imports): Option[Seq[ImportData]] = Some(s.value)

  /**
    * Constructs an `Imports` object from one or more loose sequence of imports
    *
    * Figures out which imports will get stomped over by future imports
    * before they get used, and just ignore those.
    */
  def apply(importss: Seq[ImportData]*): Imports = {
    // We iterate over the combined reversed imports, keeping track of the
    // things that will-be-stomped-over-in-the-non-reversed-world in a map.
    // If an import's target destination will get stomped over we ignore it
    //
    // At the end of the day we re-reverse the trimmed list and return it.
    val importData = importss.flatten
    val stompedTypes = mutable.Set.empty[Name]
    val stompedTerms = mutable.Set.empty[Name]
    val out = mutable.Buffer.empty[ImportData]
    for (data <- importData.reverseIterator) {
      val stomped = (data.importType: @unchecked) match {
        case ImportData.Term => Seq(stompedTerms)
        case ImportData.Type => Seq(stompedTypes)
        case ImportData.TermType => Seq(stompedTerms, stompedTypes)
      }
      if (!stomped.exists(_(data.toName))) {
        out.append(data)
        stomped.foreach(_.add(data.toName))
        data.prefix.headOption.foreach(stompedTerms.remove)
      }
    }
    new Imports(out.reverse)
  }
}

/**
  * Represents a single identifier in Scala source code, e.g. "scala" or
  * "println" or "`Hello-World`".
  *
  * Holds the value "raw", with all special characters intact, e.g.
  * "Hello-World". Can be used [[backticked]] e.g. "`Hello-World`", useful for
  * embedding in Scala source code, or [[encoded]] e.g. "Hello$minusWorld",
  * useful for accessing names as-seen-from the Java/JVM side of thigns
  */
case class Name(raw: String) {
  assert(
    NameTransformer.decode(raw) == raw,
    "Name() must be created with un-encoded text"
  )
  assert(raw.charAt(0) != '`', "Cannot create already-backticked identifiers")
  override def toString = s"Name($backticked)"
  def encoded = NameTransformer.encode(raw)
  def backticked = Name.backtickWrap(raw)
}

object Name {

  /**
    * Read/write [[Name]]s as unboxed strings, in order to save verbosity
    * in the JSON cache files as well as improving performance of
    * reading/writing since we read/write [[Name]]s a *lot*.
    */
  implicit val nameRW: upickle.default.ReadWriter[Name] =
    upickle.default.ReadWriter[Name](
      name => upickle.Js.Str(name.raw),
      { case upickle.Js.Str(raw) => Name(raw) }
    )

  val alphaKeywords = Set(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "extends",
    "false",
    "finally",
    "final",
    "finally",
    "forSome",
    "for",
    "if",
    "implicit",
    "import",
    "lazy",
    "match",
    "new",
    "null",
    "object",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "this",
    "throw",
    "trait",
    "try",
    "true",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield",
    "_",
    "macro"
  )
  val symbolKeywords = Set(
    ":",
    ";",
    "=>",
    "=",
    "<-",
    "<:",
    "<%",
    ">:",
    "#",
    "@",
    "\u21d2",
    "\u2190"
  )

  /**
    * Custom implementation of ID parsing, instead of using the ScalaParse
    * version. This lets us avoid loading FastParse and ScalaParse entirely if
    * we're running a cached script, which shaves off 200-300ms of startup time.
    */
  def backtickWrap(s: String) = {
    if (s.isEmpty) "``"
    else if (s(0) == '`' && s.last == '`') s
    else {
      val chunks = s.split("_", -1)
      def validOperator(c: Char) = {
        c.getType == Character.MATH_SYMBOL ||
        c.getType == Character.OTHER_SYMBOL ||
        "!#%&*+-/:<=>?@\\^|~".contains(c)
      }
      val validChunks = chunks.zipWithIndex.forall {
        case (chunk, index) =>
          chunk.forall(c => c.isLetter || c.isDigit || c == '$') ||
            (chunk.forall(validOperator) &&
              // operators can only come last
              index == chunks.length - 1 &&
              // but cannot be preceded by only a _
              !(chunks.lift(index - 1).exists(_ == "") && index - 1 == 0))
      }

      val firstLetterValid = s(0).isLetter || s(0) == '_' || s(0) == '$' || validOperator(
          s(0))
      val valid =
        validChunks &&
          firstLetterValid &&
          !alphaKeywords.contains(s) &&
          !symbolKeywords.contains(s)

      if (valid) s else '`' + s + '`'
    }
  }
}

/**
  * Encapsulates a read-write cell that can be passed around
  */
trait StableRef[T] {

  /**
    * Get the current value of the this [[StableRef]] at this instant in time
    */
  def apply(): T

  /**
    * Set the value of this [[StableRef]] to always be the value `t`
    */
  def update(t: T): Unit
}

trait Ref[T] extends StableRef[T] {

  /**
    * Return a function that can be used to get the value of this [[Ref]]
    * at any point in time
    */
  def live(): () => T

  /**
    * Set the value of this [[Ref]] to always be the value of the by-name
    * argument `t`, at any point in time
    */
  def bind(t: => T): Unit
}

object Ref {
  implicit def refer[T](t: T): Ref[T] = Ref(t)
  implicit def refPPrint[T: PPrint]: PPrinter[Ref[T]] = PPrinter {
    (ref, cfg) =>
      Iterator(cfg.colors.prefixColor("Ref").render, "(") ++
        implicitly[PPrint[T]].pprinter.render(ref(), cfg) ++
        Iterator(")")
  }
  def live[T](value0: () => T) = new Ref[T] {
    var value: () => T = value0
    def live() = value
    def apply() = value()
    def update(t: T) = value = () => t
    def bind(t: => T): Unit = value = () => t
    override def toString = s"Ref($value)"
  }
  def apply[T](value0: T) = live(() => value0)
}

/**
  * Nice pattern matching for chained exceptions
  */
object Ex {
  def unapplySeq(t: Throwable): Option[Seq[Throwable]] = {
    def rec(t: Throwable): List[Throwable] = {
      t match {
        case null => Nil
        case t => t :: rec(t.getCause)
      }
    }
    Some(rec(t))
  }
}

trait CodeColors {
  def ident: fansi.Attrs
  def `type`: fansi.Attrs
  def literal: fansi.Attrs
  def comment: fansi.Attrs
  def keyword: fansi.Attrs
}

/**
  * A set of colors used to highlight the miscellanious bits of the REPL.
  * Re-used all over the place in PPrint, TPrint, syntax highlighting,
  * command-echoes, etc. in order to keep things consistent
  *
  * @param prompt The command prompt
  * @param ident Definition of top-level identifiers
  * @param `type` Definition of types
  * @param literal Strings, integers and other literal expressions
  * @param prefix The Seq/Foo when printing a Seq(...) or case class Foo(...)
  * @param selected The color of text selected in the line-editor
  * @param error The color used to print error messages of all kinds
  */
case class Colors(prompt: Ref[fansi.Attrs],
                  ident: Ref[fansi.Attrs],
                  `type`: Ref[fansi.Attrs],
                  literal: Ref[fansi.Attrs],
                  prefix: Ref[fansi.Attrs],
                  comment: Ref[fansi.Attrs],
                  keyword: Ref[fansi.Attrs],
                  selected: Ref[fansi.Attrs],
                  error: Ref[fansi.Attrs],
                  warning: Ref[fansi.Attrs])
object Colors {

  def Default = Colors(
    fansi.Color.Magenta,
    fansi.Color.Cyan,
    fansi.Color.Green,
    fansi.Color.Green,
    fansi.Color.Yellow,
    fansi.Color.Blue,
    fansi.Color.Yellow,
    fansi.Reversed.On,
    fansi.Color.Red,
    fansi.Color.Yellow
  )
  def BlackWhite = Colors(
    fansi.Attrs.Empty,
    fansi.Attrs.Empty,
    fansi.Attrs.Empty,
    fansi.Attrs.Empty,
    fansi.Attrs.Empty,
    fansi.Attrs.Empty,
    fansi.Attrs.Empty,
    fansi.Attrs.Empty,
    fansi.Attrs.Empty,
    fansi.Attrs.Empty
  )
}

/**
  * Models a binding of a value to a typed name, and is passed into the
  * REPL so it can re-create the bindings inside the REPL's scope
  */
case class Bind[T](name: String, value: T)(
    implicit val typeTag: scala.reflect.runtime.universe.TypeTag[T])
object Bind {
  implicit def ammoniteReplArrowBinder[T](t: (String, T))(
      implicit typeTag: TypeTag[T]) = {
    Bind(t._1, t._2)(typeTag)
  }
}

/**
  * Encapsulates the ways the Ammonite REPL prints things. Does not print
  * a trailing newline by default; you have to add one yourself.
  *
  * @param out How you want it to print streaming fragments of stdout
  * @param warning How you want it to print a compile warning
  * @param error How you want it to print a compile error
  * @param info How you want to print compile info logging. *Not* the same
  *             as `out`, which is used to print runtime output.
  */
class Printer(val warning: String => Unit,
              val error: String => Unit,
              val info: String => Unit)

class PrinterX(val out: String => Unit,
               warning: String => Unit,
               error: String => Unit,
               info: String => Unit)
    extends Printer(warning, error, info)

case class ImportTree(prefix: Seq[String],
                      mappings: Option[ImportTree.ImportMapping],
                      start: Int,
                      end: Int)

object ImportTree {
  type ImportMapping = Seq[(String, Option[String])]
}
