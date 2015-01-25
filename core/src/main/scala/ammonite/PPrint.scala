
package ammonite
import collection.immutable

case class PConfig(maxDepth: Int = 100, depth: Int = 0, color: Boolean = false)
object PConfig{
  implicit val default = PConfig()
}

/**
 * Prettyprint a strongly-typed value, falling back to toString
 * if you don't know what to do with it. Generally used for human-facing
 * output
 */
object PPrint extends PPrintGen with LowPriPPrint{
  def defaultColorRepr[T] = new PPrint[T]{
    def colored(color: Boolean, s: String) = {
      if (!color) s
      else Console.GREEN + s + Console.RESET
    }
    def render(t: T, c: PConfig) =  colored(c.color, ""+t)
  }

  implicit val ByteRepr = defaultColorRepr[Byte]
  implicit val ShortRepr = defaultColorRepr[Short]
  implicit val IntRepr = defaultColorRepr[Int]
  implicit val LongRepr = defaultColorRepr[Long].map(_+"L")
  implicit val FloatRepr = defaultColorRepr[Float].map(_+"F")
  implicit val DoubleRepr = defaultColorRepr[Double]
  implicit val CharRepr = PPrint.make[Char](x => "'" + escape(x.toString) + "'", Some(Console.GREEN))
  implicit val StringRepr = PPrint.make[String](x => '"' + escape(x) + '"', Some(Console.GREEN))
  implicit val SymbolRepr = PPrint.make[Symbol]("'" + _.name, Some(Console.GREEN))
  implicit def ArrayRepr[T: PPrint] = new PPrint[Array[T]]{
    def repr = collectionRepr[T, Seq[T]]("Array")
    def render(t: Array[T], c: PConfig) = repr.render(t, c)
  }

  implicit def StreamRepr[T: PPrint] = collectionRepr[T, Stream[T]]("Stream")
  implicit def IterableRepr[T: PPrint] = collectionRepr[T, Iterable[T]]("Iterable")
  implicit def VectorRepr[T: PPrint] = collectionRepr[T, Vector[T]]("Vector")
  implicit def SeqRepr[T: PPrint] = collectionRepr[T, Seq[T]]("Seq")
  implicit def ListRepr[T: PPrint] = collectionRepr[T, List[T]]("List")
  implicit def TraversableRepr[T: PPrint] = collectionRepr[T, Traversable[T]]("Traversable")
  implicit def SetRepr[T: PPrint] = collectionRepr[T, Set[T]]("Set")
  implicit def SortedSetRepr[T: PPrint] = collectionRepr[T, immutable.SortedSet[T]]("immutable.SortedSet")
  implicit def MapRepr[T: PPrint, V: PPrint]: PPrint[Map[T, V]] = makeMapRepr[Map, T, V]("Map")
  implicit def SortedMapRepr[T: PPrint, V: PPrint]: PPrint[immutable.SortedMap[T, V]] = makeMapRepr[immutable.SortedMap, T, V]("immutable.SortedMap")

  def makeMapRepr[M[T, V] <: Map[T, V], T: PPrint, V: PPrint](name: String) = new PPrint[M[T, V]] {
    def repr(implicit c: PConfig) = collectionRepr[(T, V), Iterable[(T, V)]](name)(
      PPrint.make[(T, V)]{case (t, v) => PPrint(t) + " -> " + PPrint(v)}
    )
    def render(t: M[T, V], c: PConfig) = {
      val x = t.toIterable
      repr(c).render(x, c)
    }
  }
//  implicit def arrayRepr[T: PPrint] = collectionRepr[T, Array[T]]
  def make[T](f: T => String, color: Option[String] = None): PPrint[T] = new PPrint[T]{
    def render(t: T, c: PConfig) =  color.filter(_ => c.color).fold(f(t))(_ + f(t) + Console.RESET)
    def vertical(t: T, c: PConfig) = render(t, c)
  }
  def apply[T: PPrint](t: T)(implicit c: PConfig): String = {
    implicitly[PPrint[T]].render(t, c)
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

  def collectionRepr[T: PPrint, V <: Traversable[T]](name0: String): PPrint[V] = new PPrint[V] {
    def render(i: V, c: PConfig): String = {
      val chunks = i.map(implicitly[PPrint[T]].render(_, c))
      lazy val chunks2 = i.map(implicitly[PPrint[T]].render(_, c.copy(depth = c.depth + 1)))
      handleChunks(name0, chunks, chunks2, c)
    }
  }
  def handleChunks(name0: String,
                   chunks: Traversable[String],
                   chunks2: Traversable[String],
                   c: PConfig) = {
    val totalLength = name0.length + chunks.map(_.length + 2).sum

    val name =
      if (c.color) Console.YELLOW + name0 + Console.RESET
      else name0

    if (totalLength <= c.maxDepth - c.depth && !chunks.exists(_.contains('\n'))){
      name + "(" + chunks.mkString(", ") + ")"
    } else {

      val indent = "  " * c.depth
      name + "(\n" + chunks2.map("  " + indent + _).mkString(",\n") + "\n" + indent + ")"
    }
  }
}

trait PPrint[T] { outer =>
  def render(t: T, c: PConfig): String

  def map(f: String => String) = new PPrint[T] {
    def render(t: T, c: PConfig) = f(outer.render(t, c))
  }
}

trait LowPriPPrint{
  implicit def defaultRepr[T] = PPrint.make[T](""+_)
}
