package ammonite.repl

import java.security.MessageDigest

import ammonite.ops._
import acyclic.file
import fansi.Attrs
import pprint.{PPrint, PPrinter}

import scala.collection.mutable
import scala.reflect.runtime.universe.TypeTag
import scala.util.Try

/**
  * The result of a single pass through the ammonite REPL. Results in a single
  * [[T]], or of one of a fixed number of failures that are common for anyone
  * who is evaluating code via the REPL.
  */
sealed abstract class Res[+T]{
  def flatMap[V](f: T => Res[V]): Res[V]
  def map[V](f: T => V): Res[V]
  def filter(f: T => Boolean): Res[T] = this
}



object Res{
  def apply[T](o: Option[T], errMsg: => String) = o match{
    case Some(s) => Success(s)
    case None => Failure(None, errMsg)
  }
  def apply[T](o: Try[T], errMsg: Throwable => String) = o match{
    case util.Success(s) => Success(s)
    case util.Failure(t) => Failure(None, errMsg(t))
  }

  /**
    * Successes map and flatmap just like a simple Box[T]
    */
  case class Success[+T](s: T) extends Res[T] {
    def flatMap[V](f: T => Res[V]): Res[V] = f(s) match {
      case Success(v) => Success(v)
      case other => other
    }

    def map[V](f: T => V): Res[V] = Success(f(s))
  }

  /**
    * Failing results never call their callbacks, and just remain unchanged
    */
  sealed abstract class Failing extends Res[Nothing]{
    def flatMap[V](f: Nothing => Res[V]): Res[V] = this
    def map[V](f: Nothing => V): Res[V] = this
  }

  /**
    * A known failure occured, maybe caused by an exception
    * (e.g. `ThreadDeath`) and maybe not (e.g. compile error)
    *
    * @param ex is any exception that caused this known failure; currently
    *           only used for the "Interrupted!" failures caused by Ctrl-C
    * @param msg the message we want to display on screen due to this failure
    */
  case class Failure(ex: Option[Throwable], msg: String) extends Failing

  /**
    * An unknown exception was thrown when the command was being run
    */
  case class Exception(t: Throwable, s: String) extends Failing

  /**
    * Nothing was entered
    */
  case object Skip extends Failing

  /**
    * The user wanted to exit the REPL
    */
  case class Exit(value: Any) extends Failing
}


/**
  * Exception for reporting script compilation failures
  */
class CompilationError(message: String) extends Exception(message)

/**
  * Fake for-comprehension generator to catch errors and turn
  * them into [[Res.Failure]]s
  */
case class Catching(handler: PartialFunction[Throwable, Res.Failing]) {

  def foreach[T](t: Unit => T): T = t(())
  def flatMap[T](t: Unit => Res[T]): Res[T] =
    try{t(())} catch handler
  def map[T](t: Unit => T): Res[T] =
    try Res.Success(t(())) catch handler
}

case class Evaluated(wrapper: String,
                     imports: Imports)

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
case class ImportData(fromName: String,
                      toName: String,
                      prefix: String,
                      importType: ImportData.ImportType)

/**
  * Represents the imports that occur before a piece of user code in the
  * Ammonite REPL. It's basically a `Seq[ImportData]`, except we really want
  * it to be always in a "canonical" form without shadowed/duplicate imports.
  *
  * Thus we only expose an `apply` method which performs this de-duplication,
  * and a `++` operator that combines two sets of imports while performing
  * de-duplication.
  */
class Imports private (val value: Seq[ImportData]){
  def ++(others: Imports) = Imports(this.value, others.value)
}

object Imports{
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
    val stompedTypes = mutable.Set.empty[String]
    val stompedTerms = mutable.Set.empty[String]
    val out = mutable.Buffer.empty[ImportData]
    for(data <- importData.reverseIterator){
      val stomped = data.importType match{
        case ImportData.Term => Seq(stompedTerms)
        case ImportData.Type => Seq(stompedTypes)
        case ImportData.TermType => Seq(stompedTerms, stompedTypes)
      }
      if (!stomped.exists(_(data.toName))){
        out.append(data)
        stomped.foreach(_.add(data.toName))
        stompedTerms.remove(data.prefix)
      }
    }
    new Imports(out.reverse)
  }
}

object ImportData{
  sealed case class ImportType(name: String)
  val Type = ImportType("Type")
  val Term = ImportType("Term")
  val TermType = ImportType("TermType")
}

/**
 * Encapsulates a read-write cell that can be passed around
 */
trait StableRef[T]{
  /**
   * Get the current value of the this [[StableRef]] at this instant in time
   */
  def apply(): T

  /**
   * Set the value of this [[StableRef]] to always be the value `t`
   */
  def update(t: T): Unit
}

trait Ref[T] extends StableRef[T]{
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

object Ref{
  implicit def refer[T](t: T): Ref[T] = Ref(t)
  implicit def refPPrint[T: PPrint]: PPrinter[Ref[T]] = PPrinter{ (ref, cfg) =>
    Iterator(cfg.colors.prefixColor("Ref").render, "(") ++
    implicitly[PPrint[T]].pprinter.render(ref(), cfg) ++
    Iterator(")")
  }
  def live[T](value0: () => T) = new Ref[T]{
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
object Ex{
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

object Util{

  def pathToPackageWrapper(path: Path, wd: Path) = {
    val pkg = {
      val base = Seq("$script")
      val relPath = (path/up).relativeTo(wd)
      val ups = Seq.fill(relPath.ups)("$up")
      val rest = relPath.segments
      (base ++ ups ++ rest).map(scala.reflect.NameTransformer.encode).mkString(".").toLowerCase
    }
    val wrapper = scala.reflect.NameTransformer.encode(path.last.take(path.last.lastIndexOf('.')))
    (pkg, wrapper)
  }
  def md5Hash(data: Iterator[Array[Byte]]) = {
    val digest = MessageDigest.getInstance("MD5")
    data.foreach(digest.update)
    digest.digest()
  }

  // Type aliases for common things
  type IvyMap = Map[(String, String, String, String), Set[String]]
  type ClassFiles = Traversable[(String, Array[Byte])]
  type CompileCache = (ClassFiles, Imports)


  def transpose[A](xs: List[List[A]]): List[List[A]] = {
    @scala.annotation.tailrec
    def transpose(xs: List[List[A]], result: List[List[A]]): List[List[A]] = {
      xs.filter(_.nonEmpty) match {
        case Nil    =>  result
        case ys: List[List[A]] => transpose(ys.map(_.tail), ys.map(_.head) :: result)
      }
    }

    transpose(xs, Nil).reverse
  }
}

object Timer{
  var current = 0L
  var show = false
  def reset() = current = System.nanoTime()
  /**
   * Prints the time, in millis, that has passed
   * since the last time `reset` or `apply` was called
   */
  def apply(s: String) = {
    val now = System.nanoTime()
      if(show) println(s + ": " + (now - current) / 1000000.0)
    current = now
  }
}


trait CodeColors{
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
object Colors{

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
    fansi.Attrs.Empty, fansi.Attrs.Empty, fansi.Attrs.Empty, fansi.Attrs.Empty,
    fansi.Attrs.Empty, fansi.Attrs.Empty, fansi.Attrs.Empty, fansi.Attrs.Empty,
    fansi.Attrs.Empty, fansi.Attrs.Empty
  )
}

/**
 * Models a binding of a value to a typed name, and is passed into the
 * REPL so it can re-create the bindings inside the REPL's scope
 */
case class Bind[T](name: String, value: T)
                  (implicit val typeTag: scala.reflect.runtime.universe.TypeTag[T])
object Bind{
  implicit def ammoniteReplArrowBinder[T](t: (String, T))(implicit typeTag: TypeTag[T]) = {
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
case class Printer(out: String => Unit,
                   warning: String => Unit,
                   error: String => Unit,
                   info: String => Unit)
