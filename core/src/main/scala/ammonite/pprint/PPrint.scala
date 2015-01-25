package ammonite.pprint
import scala.annotation.unchecked.uncheckedVariance
import scala.language.experimental.macros
import acyclic.file


object PPrint extends Internals.LowPriPPrint{
  /**
   * Prettyprint a strongly-typed value, falling back to toString
   * if you don't know what to do with it. Generally used for human-facing
   * output
   */
  def apply[T: PPrint](t: T): String = {
    val pprint = implicitly[PPrint[T]]
    pprint.render(t, pprint.cfg)
  }

  implicit def Contra[A](implicit ca: PPrinter[A], cfg: Config): PPrint[A] = new PPrint(ca, cfg)
}

/**
 * A typeclass necessary to prettyprint something. Separate from [[PPrinter]]
 * in order to make contravariant implicit resolution behave right.
 */
class PPrint[A](val a: PPrinter[A], val cfg: Config){
  def render(t: A, c: Config) = {
    if (t == null) "null"
    else a.render(t, c)
  }
  def map(f: String => String) = a.map(f)
}

/**
 * A typeclass you define to prettyprint values of type [[A]]
 */
trait PPrinter[-A] {
  def render(t: A, c: Config): String

  def map(f: String => String): PPrinter[A] = PPrinter {
    (t: A, c: Config) => f(render(t, c))
  }  
}

object PPrinter {
  def apply[T](r: (T, Config) => String): PPrinter[T] = {
    new PPrinter[T]{def render(t: T, c: Config)= r(t, c)}
  }

  /**
   * A basic [[PPrinter]] that does `toString` and nothing else
   */
  def Literal: PPrinter[Any] = PPrinter((t, c) => t.toString)

  /**
   * A [[PPrinter]] that does `toString`, with an optional
   * color
   */
  def literalColorPPrinter[T]: PPrinter[T] = PPrinter[T] { (t: T, c: Config) =>
    c.color.literal("" + t)
  }

  implicit val ByteRepr = literalColorPPrinter[Byte]
  implicit val ShortRepr = literalColorPPrinter[Short]
  implicit val IntRepr = literalColorPPrinter[Int]
  implicit val LongRepr = literalColorPPrinter[Long].map(_+"L")
  implicit val FloatRepr = literalColorPPrinter[Float].map(_+"F")
  implicit val DoubleRepr = literalColorPPrinter[Double]
  implicit val CharRepr = PPrinter[Char]((x, c) =>
    c.color.literal("'" + escape(x.toString) + "'")
  )
  implicit val StringRepr = PPrinter[String]((x, c) =>
    c.color.literal('"' + escape(x) + '"')
  )
  implicit val SymbolRepr = PPrinter[Symbol]((x, c) =>
    c.color.literal("'" + x.name)
  )

  def make3[T](prefix: T => String)(f: Internals.Unpacker[T]): PPrinter[T] = PPrinter[T]{
    (t: T, c: Config) =>
      val (chunks, chunks2) = f.f(t, c)
      val rendered = Internals.handleChunks(prefix(t), chunks, chunks2, c)
      rendered
  }

  /**
   * Escapes a string to turn it back into a string literal
   */
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
  implicit def ArrayRepr[T: PPrint] = PPrinter[Array[T]]{
    def repr = Internals.collectionRepr[T, Seq[T]]("Array")
    (t: Array[T], c: Config) => repr.render(t, c)
  }

  implicit def SeqRepr[T: PPrint] = Internals.collectionRepr[T, Seq[T]]("Seq")
  implicit def SetRepr[T: PPrint] = Internals.collectionRepr[T, Set[T]]("Set")
  implicit def MapRepr[T: PPrint, V: PPrint] = Internals.makeMapRepr[Map, T, V]("Map")

}

object Internals {
  def makeMapRepr[M[T, V] <: Map[T, V], T: PPrint, V: PPrint](name: String) = PPrinter[M[T, V]] {
    def repr(implicit c: Config) = collectionRepr[(T, V), Iterable[(T, V)]](name) {
      val pprinter = PPrinter[(T, V)] { case ((t, v), _) =>
        implicitly[PPrint[T]].render(t, c) + " -> " + implicitly[PPrint[V]].render(v, c)
      }
      new PPrint(pprinter, c)
    }
    (t: M[T, V], c: Config) => {
      val x = t.toIterable
      repr(c).render(x, c)
    }
  }

  def collectionRepr[T: PPrint, V <: Traversable[T]](name0: String): PPrinter[V] = PPrinter[V] {
    (i: V, c: Config) => {
      val chunks = i.map(implicitly[PPrint[T]].render(_, c))
      lazy val chunks2 = i.map(implicitly[PPrint[T]].render(_, c.copy(depth = c.depth + 1)))
      handleChunks(i.stringPrefix, chunks, () => chunks2, c)
    }
  }

  /**
   * Renders something that looks like
   *
   * Prefix(inner, inner, inner)
   *
   * or
   *
   * Prefix(
   *   inner,
   *   inner,
   *   inner
   * )
   *
   * And deals with the necessary layout considerations to
   * decide whether to go vertical or horiozontal
   */
  def handleChunks(name: String,
                   chunks: Traversable[String],
                   chunks2: () => Traversable[String],
                   c: Config): String = {
    val renamed = c.rename(name)
    val totalLength = renamed.length + chunks.map(_.length + 2).sum
    val coloredName = c.color.prefix(renamed)

    if (totalLength <= c.maxDepth - c.depth && !chunks.exists(_.contains('\n'))) {
      coloredName + "(" + chunks.mkString(", ") + ")"
    } else {
      val indent = "  " * c.depth
      coloredName + "(\n" + chunks2().map("  " + indent + _).mkString(",\n") + "\n" + indent + ")"
    }
  }

  def preMap[T, V: PPrint](f: T => V) = PPrinter[T] {
    (t: T, c: Config) => implicitly[PPrint[V]].render(f(t), c)
  }
  trait Unpacker[T] {
    def f(t: T, cfg: Config): (Seq[String], () => Seq[String])
  }

  object Unpacker extends PPrinterGen {
    // Things being injected into PPrinterGen to keep it acyclic
    type UP[T] = Unpacker[T]
    type PP[T] = PPrint[T]
    type C = Config

    def make[T](r: (T, Config) => (Seq[String], () => Seq[String])): Unpacker[T] = {
      new Unpacker[T] {
        def f(t: T, cfg: Config) = r(t, cfg)
      }
    }

    def render[T: PP](t: T, c: Config) = implicitly[PPrint[T]].render(t, c)

  }

  trait LowPriPPrint {
    implicit def FinalRepr[T]: PPrint[T] = macro LowerPriPPrint.FinalRepr[T]
  }

  object LowerPriPPrint {
    def FinalRepr[T: c.WeakTypeTag](c: scala.reflect.macros.blackbox.Context) = c.Expr[PPrint[T]] {
      import c.universe._

      val tpe = c.weakTypeOf[T]
      util.Try(c.weakTypeOf[T].typeSymbol.asClass) match {

        case util.Success(f) if f.isCaseClass && !f.isModuleClass =>

          val constructor = tpe.member(newTermName("<init>"))
          val arity =
            constructor
              .typeSignature
              .paramLists
              .flatten
              .length

          val companion = tpe.typeSymbol.companion

          val paramTypes =
            constructor
              .typeSignatureIn(tpe)
              .paramLists
              .flatten
              .map(_.typeSignature)


          import compat._
          val implicits =
            paramTypes map (t =>
              c.inferImplicitValue(
                typeOf[PPrint[Int]] match {
                  case TypeRef(pre, tpe, args) =>
                    TypeRef(pre, tpe, List(t))
                }
              )
            )

          val tupleName = newTermName(s"Product${arity}Unpacker")
          val thingy =
            if (arity > 1) q"$companion.unapply(t).get"
            else q"Tuple1($companion.unapply(t).get)"
          // We're fleshing this out a lot more than necessary to help
          // scalac along with its implicit search, otherwise it gets
          // confused and explodes

          // Need to dropWhile to get rid of any `Tuple1` prefix
          val res = q"""
            new ammonite.pprint.PPrint[$tpe](
              ammonite.pprint.PPrinter.make3[$tpe](_.productPrefix){
                new ammonite.pprint.Internals.Unpacker[$tpe]{
                  def f(t: $tpe, cfg: Config) = ammonite.pprint.Internals.Unpacker.$tupleName[..$paramTypes].f(
                    $thingy,
                    cfg
                  )
                }
              },
              implicitly[Config]
            )
          """
          //        println(res)
          res

        case _ =>
          q"""new PPrint[$tpe](
            ammonite.pprint.PPrinter.Literal,
            implicitly[ammonite.pprint.Config]
          )"""
      }
    }
  }

}