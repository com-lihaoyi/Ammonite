package ammonite.terminal
import acyclic.file
object Filter{
  def apply(f: PartialFunction[TermInfo, TermAction])
           (implicit i: sourcecode.Enclosing): Filter =
    new Filter{
      val op = f.lift
      override def toString = i.value
    }

  def wrap(f: TermInfo => Option[TermAction])
          (implicit i: sourcecode.Enclosing): Filter =
    new Filter{
      val op = f
      override def toString = i.value
    }

  def merge(pfs: Filter*)
           (implicit i: sourcecode.Enclosing) = new Filter {

    val op = (v1: TermInfo) => pfs.iterator.map(_.op(v1)).find(_.isDefined).flatten

    override def toString = i.value
  }
  val empty = Filter.merge()
}
trait Filter{
  val op: TermInfo => Option[TermAction]

}
abstract class DelegateFilter(implicit val enclosing: sourcecode.Enclosing) extends Filter{
  def filter: Filter
  val op = filter.op
  override def toString = enclosing.value
}