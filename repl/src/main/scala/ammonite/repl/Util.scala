package ammonite.repl

import acyclic.file
import pprint.{PPrinter, PPrint}

import scala.util.Try
import scalaparse.Scala._

object Res{
  def apply[T](o: Option[T], errMsg: => String) = o match{
    case Some(s) => Success(s)
    case None => Failure(errMsg)
  }
  def apply[T](o: Try[T], errMsg: Throwable => String) = o match{
    case util.Success(s) => Success(s)
    case util.Failure(t) => Failure(errMsg(t))
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
  case class Failure(s: String) extends Failing
  object Failure{
    def highlightFrame(f: StackTraceElement) = {
      import Console._
      val src =
        if (f.isNativeMethod) GREEN + "Native Method" + RED
        else s"$GREEN${f.getFileName}$RED:$GREEN${f.getLineNumber}$RED"

      val prefix :+ clsName = f.getClassName.split('.').toSeq
      val prefixString = prefix.map(_+'.').mkString("")
      val clsNameString = clsName.replace("$", RED+"$"+YELLOW)
      val method = s"$RED$prefixString$YELLOW$clsNameString$RED.${f.getMethodName}"
      s"\t$method($src)"
    }
    def apply(exceptions: Seq[Throwable], stop: String = null): Failure = {
      val traces = exceptions.map(exception =>
        exception.toString + "\n" +
        exception
          .getStackTrace
          .takeWhile(x => !(x.getMethodName == stop))
          .map(highlightFrame)
          .mkString("\n")
      )
      Res.Failure(traces.mkString("\n"))
    }
  }
  case object Skip extends Failing
  case object Exit extends Failing
}

/**
 * The result of a single pass through the ammonite REPL.
 */
sealed abstract class Res[+T]{
  def flatMap[V](f: T => Res[V]): Res[V]
  def map[V](f: T => V): Res[V]
  def filter(f: T => Boolean): Res[T] = this
}

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
                     imports: Seq[ImportData])

case class ImportData(fromName: String,
                      toName: String,
                      wrapperName: String,
                      prefix: String)

/**
 * Encapsulates a read-write cell that can be passed around
 */
trait Ref[T]{
  def apply(): T
  def update(t: T): Unit
}
object Ref{
  implicit def refPPrint[T: PPrint]: PPrinter[Ref[T]] = PPrinter{ (ref, cfg) =>
    Iterator(cfg.colors.prefixColor, "Ref", cfg.colors.endColor, "(") ++
    implicitly[PPrint[T]].pprinter.render(ref(), cfg) ++
    Iterator(")")
  }
  def apply[T](value0: T) = {
    var value = value0
    new Ref[T]{
      def apply() = value
      def update(t: T) = value = t
    }
  }
  def apply[T](value: => T, update0: T => Unit) = new Ref[T]{
    def apply() = value
    def update(t: T) = update0(t)
  }
}
trait Cell[T]{
  def apply(): T
  def update(): Unit
}
object Cell{
  class LazyHolder[T](t: => T){
    lazy val value = t
  }
  def apply[T](t: => T) = new Cell[T] {
    var inner: LazyHolder[T] = new LazyHolder(t)
    def update() = inner = new LazyHolder(t)
    def apply() = inner.value
  }
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
  type IvyMap = Map[(String, String, String), Set[String]]
  def transpose[A](xs: List[List[A]]): List[List[A]] = xs.filter(_.nonEmpty) match {
    case Nil    =>  Nil
    case ys: List[List[A]] => ys.map{ _.head }::transpose(ys.map{ _.tail })
  }
}

object Timer{
  var current = 0L
  def reset() = current = System.nanoTime()
  /**
   * Prints the time, in millis, that has passed
   * since the last time `reset` or `apply` was called
   */
  def apply(s: String) = {
    val now = System.nanoTime()
    println(s + ": " + (now - current) / 1000000.0)
    current = now
  }
}
object Parsers {

  import fastparse.noApi._

  import scalaparse.Scala._
  import WhitespaceApi._

  val PatVarSplitter = {
    val Prefixes = P(Prelude ~ (`var` | `val`))
    val Lhs = P( Prefixes ~! BindPattern.rep(1, "," ~! Pass) ~ (`:` ~! Type).? )
    P( Lhs.! ~ (`=` ~! WL ~ StatCtx.Expr.!) ~ End )
  }
  def patVarSplit(code: String) = {
    val Result.Success((lhs, rhs), _) = PatVarSplitter.parse(code)
    (lhs, rhs)
  }
  val Id2 = P( Id ~ End )
  def backtickWrap(s: String) = {
    Id2.parse(s) match{
      case _: Result.Success[_] => s
      case _ => "`" + pprint.PPrinter.escape(s) + "`"
    }
  }

  val Prelude = P( (Annot ~ OneNLMax).rep ~ (Mod ~! Pass).rep )
  val Statement = P ( scalaparse.Scala.Import | Prelude ~ BlockDef | StatCtx.Expr )
  def StatementBlock(blockSep: P0) = P ( Semis.? ~ (!blockSep ~ Statement).!.repX(sep=Semis) ~ Semis.? )
  val Splitter = P( StatementBlock(Fail) ~ WL ~ End)

  /**
   * Attempts to break a code blob into multiple statements. Returns `None` if
   * it thinks the code blob is "incomplete" and requires more input
   */
  def split(code: String) = Splitter.parse(code) match{
    case Result.Failure(_, index) if code.drop(index).trim() == "" => None
    case x => Some(x)
  }

  val Separator = P( WL ~ "@" ~~ CharIn(" \n").rep(1) )
  val CompilationUnit = P( WL ~ StatementBlock(Separator) ~ WL )
  val ScriptSplitter = P( CompilationUnit.repX(1, Separator) ~ End)
  def splitScript(code: String) = ScriptSplitter.parse(code).get.value

  val BlockUnwrapper = P( "{" ~ Block.! ~ "}" ~ End)
  def unwrapBlock(code: String) = {
    BlockUnwrapper.parse(code) match{
      case Result.Success(contents, _) => Some(contents)
      case _ => None
    }
  }
}
