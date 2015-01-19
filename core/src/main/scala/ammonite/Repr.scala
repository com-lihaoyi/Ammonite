package ammonite

trait Repr[T] {
  def apply(t: T): String
}
object Repr extends LowPriRepr{
  implicit val byteRepr = defaultRepr[Byte]
  implicit val shortRepr = defaultRepr[Short]
  implicit val intRepr = defaultRepr[Int]
  implicit val longRepr = defaultRepr[Long]
  implicit val floatRepr = defaultRepr[Float]
  implicit val doubleRepr = defaultRepr[Double]
  implicit val stringRepr = Repr.make[String]('"' + _ + '"')
  implicit val symbolRepr = Repr.make[Symbol](''' + _.name)
  implicit def vectorRepr[T: Repr] = collectionRepr[T, Vector[T]]("Vector")
  implicit def seqRepr[T: Repr] = collectionRepr[T, Seq[T]]("Seq")
  implicit def listRepr[T: Repr] = collectionRepr[T, List[T]]("List")
  implicit def setRepr[T: Repr] = collectionRepr[T, Set[T]]("Set")
//  implicit def arrayRepr[T: Repr] = collectionRepr[T, Array[T]]
  def make[T](f: T => String) = new Repr[T]{
    def apply(t: T) = f(t)
  }
  def apply[T: Repr](t: T) = implicitly[Repr[T]].apply(t)
}
trait LowPriRepr{
  implicit def defaultRepr[T] = Repr.make[T](_.toString)
  def collectionRepr[T: Repr, V <: Iterable[T]](name: String): Repr[V] = Repr.make[V] { i =>
    name + "(" + i.map(Repr[T]).mkString(", ") + ")"
  }
}
