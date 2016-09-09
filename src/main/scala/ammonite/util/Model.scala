/**
  * Various common, "dumb" data-structures that represent common things that
  * are passed around inside Ammonite
  */
package ammonite.util

import pprint.{PPrint, PPrinter}

import scala.reflect.runtime.universe.TypeTag

import language.implicitConversions

/**
  * Exception for reporting script compilation failures
  */
class CompilationError(message: String) extends Exception(message)

case class Evaluated(wrapper: Seq[Name], imports: Imports, tag: String)

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
  implicit def refPPrint[T: PPrint]: PPrinter[Ref[T]] = PPrinter { (ref, cfg) =>
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
case class Bind[T](name: String, value: T)(implicit val typeTag: scala.reflect.runtime.universe.TypeTag[T])
object Bind {
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
class Printer(val warning: String => Unit, val error: String => Unit, val info: String => Unit)

class PrinterX(val out: String => Unit, warning: String => Unit, error: String => Unit, info: String => Unit)
    extends Printer(warning, error, info)
