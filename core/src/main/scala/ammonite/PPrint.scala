
package ammonite
import collection.immutable
import scala.annotation.unchecked.uncheckedVariance
import scala.language.experimental.macros
import scala.reflect.ClassTag

case class PConfig(maxDepth: Int = 100,
                   depth: Int = 0,
                   color: Boolean = false,
                   renames: Map[String, String] = PConfig.defaultRenames)

object PConfig{
  val defaultRenames = Map(
    "WrappedArray" -> "Array"
  ) ++ (2 to 22).map(i => s"Tuple$i" -> "")
//  println(defaultRenames)
  implicit val default = PConfig()
}

/**
 * Prettyprint a strongly-typed value, falling back to toString
 * if you don't know what to do with it. Generally used for human-facing
 * output
 */
object PPrint extends SpecificLow{

  def apply[T: PPrint](t: T)(implicit c: PConfig): String = {
    implicitly[PPrint[T]].render(t, c)
  }

  implicit def Cov[A](implicit ca: PPrintCov[A]): PPrint[A] = PPrint(ca.selff)
}

trait PPrintContra[-A] extends PPrintCov[A @uncheckedVariance] {
  def render(t: A, c: PConfig): String

  def map(f: String => String): (PPrintContra[A] @uncheckedVariance) = PPrintContra {
    (t: A, c: PConfig) => f(render(t, c))
  }
}
object PPrintContra extends PPrintGen{
  def apply[T](r: (T, PConfig) => String): PPrintContra[T] = {
    new PPrintContra[T]{def render(t: T, c: PConfig)= r(t, c)}
  }
  def defaultColorRepr[T]: PPrintContra[T] = PPrintContra[T]{
    def colored(color: Boolean, s: String) = {
      if (!color) s
      else Console.GREEN + s + Console.RESET
    }
    (t: T, c: PConfig) => colored(c.color, ""+t)
  }

  implicit val ByteRepr = defaultColorRepr[Byte]
  implicit val ShortRepr = defaultColorRepr[Short]
  implicit val IntRepr = defaultColorRepr[Int]
  implicit val LongRepr = defaultColorRepr[Long].map(_+"L")
  implicit val FloatRepr = defaultColorRepr[Float].map(_+"F")
  implicit val DoubleRepr = defaultColorRepr[Double]
  implicit val CharRepr: PPrintContra[Char] = PPrintContra.make[Char](x => "'" + escape(x.toString) + "'", Some(Console.GREEN))
  implicit val StringRepr = PPrintContra.make[String](x => '"' + escape(x) + '"', Some(Console.GREEN))
  implicit val SymbolRepr = PPrintContra.make[Symbol]("'" + _.name, Some(Console.GREEN))

  def make[T](f: T => String, color: Option[String] = None): PPrintContra[T] = PPrintContra[T]{
    val render = (t: T, c: PConfig) => color.filter(_ => c.color).fold(f(t))(_ + f(t) + Console.RESET)
    def vertical(t: T, c: PConfig) = render(t, c)
    render
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
  implicit def ArrayRepr[T: PPrint] = PPrintContra[Array[T]]{
    def repr = collectionRepr[T, Seq[T]]("Array")
    (t: Array[T], c: PConfig) => repr.render(t, c)
  }

  implicit def SeqRepr[T: PPrint] = collectionRepr[T, Seq[T]]("Seq")
  implicit def SetRepr[T: PPrint] = collectionRepr[T, Set[T]]("Set")
  implicit def MapRepr[T: PPrint, V: PPrint]= makeMapRepr[Map, T, V]("Map")

  def makeMapRepr[M[T, V] <: Map[T, V], T: PPrint, V: PPrint](name: String) = PPrintContra[M[T, V]] {
    def repr(implicit c: PConfig) = collectionRepr[(T, V), Iterable[(T, V)]](name)(
      PPrint(PPrintContra.make[(T, V)]{case (t, v) => PPrint(t) + " -> " + PPrint(v)})
    )
    (t: M[T, V], c: PConfig) => {
      val x = t.toIterable
      repr(c).render(x, c)
    }
  }
  def collectionRepr[T: PPrint, V <: Traversable[T]](name0: String): PPrintContra[V] = PPrintContra[V] {
    (i: V, c: PConfig) => {
      val chunks = i.map(implicitly[PPrint[T]].render(_, c))
      lazy val chunks2 = i.map(implicitly[PPrint[T]].render(_, c.copy(depth = c.depth + 1)))
      handleChunks(c.renames.getOrElse(i.stringPrefix, i.stringPrefix), chunks, chunks2, c)
    }
  }

  def handleChunks(name0: String,
                   chunks: Traversable[String],
                   chunks2: Traversable[String],
                   c: PConfig): String = {
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

  def preMap[T, V: PPrint](f: T => V) = PPrintContra[T] {
    (t: T, c: PConfig) => implicitly[PPrint[V]].render(f(t), c)
  }
}
trait PPrintCov[+A] { self: PPrintContra[A @uncheckedVariance] => val selff = self }

case class PPrint[A](a: PPrintContra[A]){
  def render(t: A, c: PConfig) = a.render(t, c)
  def map(f: String => String) = a.map(f)
} 

trait SpecificLow extends LowPriPPrint{
  implicit def Contra[A](implicit ca: PPrintContra[A]): PPrint[A] = PPrint(ca)
}


/**
 * Fall back to toString if you can't find anything else
 */
trait LowPriPPrint{
  implicit def defaultRepr[T]: PPrint[T] = macro LowPriPrint.thingy[T]
}
object LowPriPrint{
  def thingy[T: c.WeakTypeTag](c: scala.reflect.macros.blackbox.Context) = c.Expr[PPrint[T]]{
    import c.universe._
    import c._
    val tpe = c.weakTypeOf[T]
    util.Try(c.weakTypeOf[T].typeSymbol.asClass) match {
      case util.Success(f) if f.isCaseClass =>

        val arity =
          tpe.member(newTermName("<init>"))
             .typeSignature
             .paramLists
             .flatten
             .length

        val companion = tpe.typeSymbol.companion

        q"""
          PPrint(ammonite.PPrintContra.preMap((t: $tpe) => $companion.unapply(t).get).map(
            ${tpe.typeSymbol.name.toString} + _
          ))
        """

      case _ =>
        q"""PPrint(ammonite.PPrintContra.make[$tpe](""+_))"""
    }
  }
}
