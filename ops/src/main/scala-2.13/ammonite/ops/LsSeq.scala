package ammonite.ops

import scala.collection.generic.IsIterable

/**
 * A specialized Seq[Path] used to provide better a better pretty-printed
 * experience
 */
case class LsSeq(base: Path, listed: RelPath*) extends Seq[Path]{
  def length = listed.length
  def apply(idx: Int) = base/listed.apply(idx)
  def iterator = listed.iterator.map(base / _)
}

object LsSeq {
  // not sure why this one is necessary
  implicit def isIterable: IsIterable[LsSeq] { type A = Path; type C = Seq[Path] } =
    new IsIterable[LsSeq] {
      type A = Path
      type C = Seq[Path]
      def apply(coll: LsSeq) =
        implicitly[IsIterable[Seq[Path]] { type A = Path; type C = Seq[Path] }].apply(coll)
    }
}
