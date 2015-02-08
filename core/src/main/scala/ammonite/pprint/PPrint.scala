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

  /**
   * Helper to make implicit resolution behave right
   */
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
  def mapEntryPrinter[T: PPrint, V: PPrint] = PPrinter[(T, V)] { case ((t, v), c) =>
    implicitly[PPrint[T]].render(t, c) + " -> " + implicitly[PPrint[V]].render(v, c)
  }
  def makeMapRepr[M[T, V] <: Map[T, V], T: PPrint, V: PPrint](name: String) = {
    PPrinter[M[T, V]] { (t: M[T, V], c: Config) =>
      handleChunks(name, c, c =>
        t.map(mapEntryPrinter[T, V].render(_, c))
      )
    }
  }

  def collectionRepr[T: PPrint, V <: Traversable[T]](name0: String): PPrinter[V] = PPrinter[V] {
    (i: V, c: Config) => {
      handleChunks(i.stringPrefix, c,
        c => i.map(implicitly[PPrint[T]].render(_, c))
      )
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
                   c: Config,
                   chunkFunc: Config => Traversable[String]): String = {
    val chunks = chunkFunc(c)
    val renamed = c.rename(name)
    val totalLength = renamed.length + chunks.map(_.length + 2).sum
    val coloredName = c.color.prefix(renamed)

    if (totalLength <= c.maxWidth - (c.depth * c.indent) && !chunks.exists(_.contains('\n'))) {
      coloredName + "(" + chunks.mkString(", ") + ")"
    } else {
      val chunks2 = chunkFunc(c.deeper)
      val indent = "  " * c.depth
      coloredName + "(\n" + chunks2.map("  " + indent + _).mkString(",\n") + "\n" + indent + ")"
    }
  }

  def preMap[T, V: PPrint](f: T => V) = PPrinter[T] {
    (t: T, c: Config) => implicitly[PPrint[V]].render(f(t), c)
  }

  type Unpacker[T] = (T, Config) => Seq[String]

  object Unpacker extends PPrinterGen {
    // Things being injected into PPrinterGen to keep it acyclic
    type UP[T] = Unpacker[T]
    type PP[T] = PPrint[T]
    type C = Config



    def render[T: PP](t: T, c: Config) = implicitly[PPrint[T]].render(t, c)
  }

  trait LowPriPPrint {
    implicit def FinalRepr[T]: PPrint[T] = macro LowerPriPPrint.FinalRepr[T]
  }

  def fromUnpacker[T](prefix: T => String)(f: Internals.Unpacker[T]): PPrinter[T] = PPrinter[T]{
    (t: T, c: Config) =>
      val rendered = Internals.handleChunks(prefix(t), c, f(t, _))
      rendered
  }

  object LowerPriPPrint {
    def FinalRepr[T: c.WeakTypeTag](c: scala.reflect.macros.blackbox.Context) = c.Expr[PPrint[T]] {
      import c.universe._

      val tpe = c.weakTypeOf[T]
      util.Try(c.weakTypeOf[T].typeSymbol.asClass) match {

        case util.Success(f) if f.isCaseClass && !f.isModuleClass =>

          val constructor = tpe.member(newTermName("<init>"))


          val companion = tpe.typeSymbol.companionSymbol

          val paramTypes =
            constructor
              .typeSignatureIn(tpe)
              .asInstanceOf[MethodType]
              .params
              .map(_.typeSignature)

          val arity = paramTypes.length

          import compat._
          val implicits =
            paramTypes.map(t =>
              c.inferImplicitValue(
                typeOf[PPrint[Int]] match {
                  case TypeRef(pre, tpe, args) =>
                    TypeRef(pre, tpe, List(t))
                }
              )
            )

          val tupleName = newTermName(s"Product${arity}Unpacker")
          val thingy ={
            val get = q"$companion.unapply(t).get"
            arity match{
              case 0 => q"()"
              case 1 => q"Tuple1($get)"
              case n => q"$companion.unapply(t).get"
            }
          }
          // We're fleshing this out a lot more than necessary to help
          // scalac along with its implicit search, otherwise it gets
          // confused and explodes

          // Need to dropWhile to get rid of any `Tuple1` prefix
          q"""
            new ammonite.pprint.PPrint[$tpe](
              ammonite.pprint.Internals.fromUnpacker[$tpe](_.productPrefix){
                (t: $tpe, cfg: ammonite.pprint.Config) =>
                  ammonite.pprint
                          .Internals
                          .Unpacker
                          .$tupleName[..$paramTypes]
                          .apply($thingy, cfg)
              },
              implicitly[ammonite.pprint.Config]
            )
          """

        case _ =>
          q"""new ammonite.pprint.PPrint[$tpe](
            ammonite.pprint.PPrinter.Literal,
            implicitly[ammonite.pprint.Config]
          )"""
      }
    }
  }

}