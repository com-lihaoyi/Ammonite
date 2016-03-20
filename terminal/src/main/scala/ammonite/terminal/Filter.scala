package ammonite.terminal
import acyclic.file
object Filter{

  def apply(f: PartialFunction[TermInfo, TermAction])
           (implicit i: sourcecode.Enclosing): Filter =
    new Filter{
      val op = f.lift
      def identifier = i.value
    }

  def wrap(f: TermInfo => Option[TermAction])
          (implicit i: sourcecode.Enclosing): Filter =
    new Filter{
      val op = f
      def identifier = i.value
    }

  /**
    * Merges multiple [[Filter]]s into one.
    */
  def merge(pfs: Filter*)
           (implicit i: sourcecode.Enclosing) = new Filter {

    val op = (v1: TermInfo) => pfs.iterator.map(_.op(v1)).find(_.isDefined).flatten

    def identifier = i.value
  }
  val empty = Filter.merge()
}

/**
  * The way you configure your terminal behavior; a trivial wrapper around a
  * function, though you should provide a good `.toString` method to make
  * debugging easier. The [[TermInfo]] and [[TermAction]] types are its
  * interface to the terminal.
  *
  * [[Filter]]s are composed sequentially: if a filter returns `None` the next
  * filter is tried, while if a filter returns `Some` that ends the cascade.
  * While your `op` function interacts with the terminal purely through
  * immutable case classes, the Filter itself is free to maintain its own state
  * and mutate it whenever, even when returning `None` to continue the cascade.
  */
trait Filter{
  val op: TermInfo => Option[TermAction]

  /**
    * the `.toString` of this object, except by making it separate we force
    * the implementer to provide something and stop them from accidentally
    * leaving it as the meaningless default.
    */
  def identifier: String
  override def toString = identifier
}

/**
  * A filter as an abstract class, letting you provide a [[filter]] instead of
  * an `op`, automatically providing a good `.toString` for debugging, and
  * providing a reasonable "place" inside the inheriting class/object to put
  * state or helpers or other logic associated with the filter.
  */
abstract class DelegateFilter(implicit val enclosing: sourcecode.Enclosing) extends Filter{
  def filter: Filter
  val op = filter.op
  def identifier = enclosing.value
}