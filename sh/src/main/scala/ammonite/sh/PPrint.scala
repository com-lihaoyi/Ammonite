package ammonite.sh
import acyclic.file
import scala.collection.{immutable => imm}

object PConfig{
  implicit val pc = new PConfig()
}
case class PConfig(maxDepth: Int = 100, depth: Int = 0)

object PPrint extends LowPriPPrint{
  implicit val ByteRepr = defaultRepr[Byte]
  implicit val ShortRepr = defaultRepr[Short]
  implicit val IntRepr = defaultRepr[Int]
  implicit val LongRepr = defaultRepr[Long].map(_+"L")
  implicit val FloatRepr = defaultRepr[Float].map(_+"F")
  implicit val DoubleRepr = defaultRepr[Double]
  implicit val CharRepr = PPrint.make[Char](x => "'" + escape(x.toString) + "'")
  implicit val StringRepr = PPrint.make[String](x => '"' + escape(x) + '"')
  implicit val SymbolRepr = PPrint.make[Symbol]("'" + _.name)
  implicit def ArrayRepr[T: PPrint] = new PPrint[Array[T]]{
    def repr = collectionRepr[T, Seq[T]]("Array")
    def render(t: Array[T], maxDepth: Int, depth: Int) = repr.render(t, maxDepth, depth)
  }
  implicit def StreamRepr[T: PPrint] = collectionRepr[T, Stream[T]]("Stream")
  implicit def IterableRepr[T: PPrint] = collectionRepr[T, Iterable[T]]("Iterable")
  implicit def VectorRepr[T: PPrint] = collectionRepr[T, Vector[T]]("Vector")
  implicit def SeqRepr[T: PPrint] = collectionRepr[T, Seq[T]]("Seq")
  implicit def ListRepr[T: PPrint] = collectionRepr[T, List[T]]("List")
  implicit def TraversableRepr[T: PPrint] = collectionRepr[T, Traversable[T]]("Traversable")
  implicit def SetRepr[T: PPrint] = collectionRepr[T, Set[T]]("Set")
  implicit def SortedSetRepr[T: PPrint] = collectionRepr[T, imm.SortedSet[T]]("imm.SortedSet")
  implicit def MapRepr[T: PPrint, V: PPrint]: PPrint[Map[T, V]] = makeMapRepr[Map, T, V]("Map")
  implicit def SortedMapRepr[T: PPrint, V: PPrint]: PPrint[imm.SortedMap[T, V]] = makeMapRepr[imm.SortedMap, T, V]("imm.SortedMap")

  def makeMapRepr[M[T, V] <: Map[T, V], T: PPrint, V: PPrint](name: String) = new PPrint[M[T, V]] {
    def repr = collectionRepr[(T, V), Iterable[(T, V)]](name)(
      PPrint.make[(T, V)]{case (t, v) => PPrint(t) + " -> " + PPrint(v)}
    )
    def render(t: M[T, V], maxDepth: Int, depth: Int) = {
      val x = t.toIterable
      repr.render(x, maxDepth, depth)
    }
  }
//  implicit def arrayRepr[T: PPrint] = collectionRepr[T, Array[T]]
  def make[T](f: T => String): PPrint[T] = new PPrint[T]{
    def render(t: T, maxDepth: Int, depth: Int) =  f(t)
    def vertical(t: T, maxDepth: Int, depth: Int) = f(t)
  }
  def apply[T: PPrint](t: T)(implicit c: PConfig): String = {
    implicitly[PPrint[T]].render(t, c.maxDepth, c.depth)
  }
  def escape(text: String): String = {
    val s = new StringBuilder
    val len = text.length
    var pos = 0
    var prev = 0

    @inline
    def handle(snip: String) = {
      s.append(text.substring(prev, pos))
      s.append(snip)
    }
    while (pos < len) {
      text.charAt(pos) match {
        case '"' => handle("\\\""); prev = pos + 1
        case '\n' => handle("\\n"); prev = pos + 1
        case '\r' => handle("\\r"); prev = pos + 1
        case '\t' => handle("\\t"); prev = pos + 1
        case '\\' => handle("\\\\"); prev = pos + 1
        case _ =>
      }
      pos += 1
    }
    handle("")
    s.toString()
  }

  def collectionRepr[T: PPrint, V <: Traversable[T]](name: String): PPrint[V] = new PPrint[V] {
    def render(i: V, maxDepth: Int, depth: Int): String = {
      val chunks = i.map(implicitly[PPrint[T]].render(_, maxDepth, depth))
      val totalLength = name.length + chunks.map(_.length + 2).sum

      if (totalLength <= maxDepth - depth && !chunks.exists(_.contains('\n'))){
        name + "(" + chunks.mkString(", ") + ")"
      } else {
        val chunks = i.map(implicitly[PPrint[T]].render(_, maxDepth, depth + 1))
        val indent = "  "*depth
        name + "(\n" + chunks.map("  " + indent + _).mkString(",\n") + "\n" + indent + ")"
      }
    }
  }
}

trait PPrint[T] { outer =>
  def render(t: T, maxDepth: Int, depth: Int): String

  def map(f: String => String) = new PPrint[T] {
    def render(t: T, maxDepth: Int, depth: Int) = f(outer.render(t, maxDepth, depth))
  }
}

trait LowPriPPrint{
  implicit def defaultRepr[T] = PPrint.make[T](""+_)
}
