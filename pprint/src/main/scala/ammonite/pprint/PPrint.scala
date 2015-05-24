package ammonite.pprint

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.generic.CanBuildFrom
import scala.language.experimental.macros
import annotation.tailrec
import acyclic.file
import scala.{Iterator => Iter}

object PPrint extends Internals.LowPriPPrint{
  /**
   * Prettyprint a strongly-typed value, falling back to toString
   * if you don't know what to do with it. Generally used for human-facing
   * output
   */
  def apply[T: PPrint](t: T): Iter[String] = {
    val pprint = implicitly[PPrint[T]]
    pprint.render(t)
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
case class PPrint[A](a: PPrinter[A], cfg: Config){
  def render(t: A): Iter[String] = {
    if (t == null) Iter("null")
    else a.render(t, cfg)
  }
  def map(f: String => String) = a.map(f)
}

/**
 * Wrapper type for disabling output truncation.
 * PPrint(Full(value)) will always return the full output.
 */
case class Show[A](a: A, lines: Int)

/**
 * A typeclass you define to prettyprint values of type [[A]]
 */
trait PPrinter[-A] {
  def render(t: A, c: Config): Iter[String]

  def map(f: String => String): PPrinter[A] = PPrinter {
    (t: A, c: Config) => render(t, c).map(f)
  }  
}

object PPrinter extends LowPriPPrinter{
  def apply[T](r: (T, Config) => Iter[String]): PPrinter[T] = {
    new PPrinter[T]{ 
      def render(t: T, c: Config) = {
        if(c.lines > 0)
          takeFirstLines(c, r(t, c))
        else r(t, c)
      }
    }
  }

  /**
   * A basic [[PPrinter]] that does `toString` and nothing else
   */
  def Literal: PPrinter[Any] = PPrinter((t, c) => Iter(t.toString))

  /**
   * A [[PPrinter]] that does `toString`, with an optional
   * color
   */
  def literalColorPPrinter[T]: PPrinter[T] = PPrinter[T] { (t: T, c: Config) =>
    Iter(c.color.literal("" + t))
  }

  implicit val ByteRepr = literalColorPPrinter[Byte]
  implicit val ShortRepr = literalColorPPrinter[Short]
  implicit val IntRepr = literalColorPPrinter[Int]
  implicit val LongRepr = literalColorPPrinter[Long].map(_+"L")
  implicit val FloatRepr = literalColorPPrinter[Float].map(_+"F")
  implicit val DoubleRepr = literalColorPPrinter[Double]
  implicit val CharRepr = PPrinter[Char]((x, c) =>
    Iter(c.color.literal("'" + escape(x.toString) + "'"))
  )

  implicit val StringRepr = PPrinter[String] { (x, c) =>
    val escaped = escape(x)
    Iter(c.color.literal(
      if (escaped.length - x.length < 1) '"' + escaped +'"'
      else {
        val indent = "  " * c.depth
        val indented = x.lines.map(indent  + _).mkString("\n")
        "\"\"\"\n" + indented + "\n" + indent  + "\"\"\""
      }
    ))
  }
  implicit val SymbolRepr = PPrinter[Symbol]((x, c) =>
    Iter(c.color.literal("'" + x.name))
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

  private def takeFirstLines(cfg: Config, iter: Iter[String]): Iter[String] = {
   
    //Calculates how many lines and characters are remaining after printing the given string.
    //Also returns how much of thsi string can be printed if the space runs out
    @tailrec
    def charIter(str: String, pos: Int, lines: Int, chars: Int): (Int, Int, Option[Int]) = {
      if(pos >= str.length) (lines, chars, None)
      else if(lines == 1 && chars == 0){
        //this would be the first character wrapping into the first line not printed
        (0, 0, Some(pos))
      }
      else{

        val (remainingLines, remainingChars) =
          if(str(pos) == '\n') (lines - 1, cfg.maxWidth) //starting a new line
          else if(chars == 0) (lines - 1, cfg.maxWidth - 1) //wrapping around and printing a character
          else (lines, chars - 1) //simply printing a character
        if(remainingLines == 0) (lines, chars, Some(pos + 1))
        else charIter(str, pos + 1, remainingLines, remainingChars)
      }
    }
   
    @tailrec
    def strIter(lines: Int, chars: Int, begin: Iter[String]): Iter[String] = {
      if(!iter.hasNext) begin
      else if(lines == 0) begin ++ Iter(cfg.color.prefix("..."))
      else{
        val head = iter.next
        val (remainingLines, remainingChars, substringLength) = charIter(head, 0, lines, chars)
        if(!substringLength.isEmpty){
          begin ++ Iter(
            head.substring(0, substringLength.get),
            cfg.color.prefix("...")
          )
        } else {
          strIter(remainingLines, remainingChars, begin ++ Iter(head))
        }
      }
    }
    strIter(cfg.lines, cfg.maxWidth, Iter.empty)
  }

  implicit def ArrayRepr[T: PPrint] = PPrinter[Array[T]]{
    def repr = Internals.collectionRepr[T, Seq[T]]
    (t: Array[T], c: Config) => repr.render(t, c)
  }



  implicit def MapRepr[T: PPrint, V: PPrint] = Internals.makeMapRepr[collection.Map, T, V]

  implicit def showPPrinter[A: PPrint]: PPrinter[Show[A]] = {
    new PPrinter[Show[A]]{
      def render(wrapper: Show[A], c: Config) = {
        val pprint = implicitly[PPrint[A]]
        pprint.copy(cfg = c.copy(lines = wrapper.lines)).render(wrapper.a)
      }
    }
  }
}
trait LowPriPPrinter{
  implicit def SeqRepr[T: PPrint, V[T] <: Traversable[T]]  =
    Internals.collectionRepr[T, V[T]]
}
object Unpacker extends PPrinterGen {
  // Things being injected into PPrinterGen to keep it acyclic
  type UP[T] = Internals.Unpacker[T]
  type PP[T] = PPrint[T]
  type C = Config

  /**
   * Special, because `Product0` doesn't exist
   */
  implicit def Product0Unpacker = (t: Unit) => Iter[Iter[String]]()
  val foo = 1
  def render[T: PP](t: T, c: Config) = implicitly[PPrint[T]].copy(cfg=c).render(t)
}


object Internals {

  def mapEntryPrinter[T: PPrint, V: PPrint] = PPrinter[(T, V)] { case ((t, v), c) =>
    implicitly[PPrint[T]].render(t) ++ Iter(" -> ") ++ implicitly[PPrint[V]].render(v)
  }
  def makeMapRepr[M[T, V] <: collection.Map[T, V], T: PPrint, V: PPrint] = {
    PPrinter[M[T, V]] { (t: M[T, V], c: Config) =>
      handleChunks(t.stringPrefix, c, { c =>
        val entryPrinter = mapEntryPrinter[T, V](
          implicitly[PPrint[T]].copy(cfg = c),
          implicitly[PPrint[V]].copy(cfg = c)
        )
        t.iterator.map(k =>
          entryPrinter.render(k, c)
        )
      })
    }
  }

  def collectionRepr[T: PPrint, V <: Traversable[T]]: PPrinter[V] = PPrinter[V] {
    (i: V, c: Config) => {
      val pp = implicitly[PPrint[T]].copy(cfg = c)
      def cFunc = (cfg: Config) => {
        i.toIterator.map(pp.copy(cfg = cfg).render)
      }
      if (!i.isInstanceOf[Stream[T]]) handleChunks(i.stringPrefix, pp.cfg, cFunc)
      else handleChunksVertical(i.stringPrefix, pp.cfg, cFunc)
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
                   chunkFunc: Config => Iter[Iter[String]]): Iter[String] = {

    val renamed = c.rename(name)
    val coloredName = c.color.prefix(renamed)
    // Prefix, contents, and all the extra ", " "(" ")" characters
    val horizontalChunks =
      chunkFunc(c).flatMap(", " +: _.toStream)
                  .toStream
                  .drop(1)
    val effectiveWidth = c.maxWidth - (c.depth * c.indent)
    // Make sure we don't read more from the `chunks` stream that we
    // have to before deciding to go vertically.
    //
    // This keeps the pprinting lazy, ensuring you can pprint arbitrarily
    // collections only ever traversing approximately the amount you need
    // before truncation, and never the whole thing
    @tailrec def checkOverflow(chunks: Stream[String], currentWidth: Int): Boolean = chunks match{
      case Stream.Empty => false
      case head #:: rest =>
        if (head.contains("\n")) true
        else {
          val nextWidth = currentWidth + head.length
          if (nextWidth > effectiveWidth) true
          else checkOverflow(rest, nextWidth)
        }
    }

    if (checkOverflow(horizontalChunks, renamed.length + 2)) handleChunksVertical(name, c, chunkFunc)
    else Iter(coloredName, "(") ++ horizontalChunks ++ Iter(")")
  }

  def handleChunksVertical(name: String,
                           c: Config,
                           chunkFunc: Config => Iter[Iter[String]]): Iter[String] = {
    val renamed = c.rename(name)
    val coloredName = c.color.prefix(renamed)
    val chunks2 = chunkFunc(c.deeper)

    // Needs to be a def to avoid exhaustion
    def indent = Iter.fill(c.depth)("  ")

    Iter(coloredName, "(\n") ++
    chunks2.flatMap(Iter(",\n", "  ") ++ indent ++ _).drop(1) ++
    Iter("\n") ++
    indent ++
    Iter(")")
  }

  type Unpacker[T] = (T, Config) => Iter[Iter[String]]


  trait LowPriPPrint {
    implicit def FinalRepr[T]: PPrint[T] = macro LowerPriPPrint.FinalRepr[T]
  }

  def fromUnpacker[T](prefix: T => String)(f: Internals.Unpacker[T]): PPrinter[T] = PPrinter[T]{
    (t: T, c: Config) =>
      Internals.handleChunks(prefix(t), c, f(t, _))
  }

  object LowerPriPPrint {
    def companionTree(c: MacroContext.Context)(tpe: c.Type) = {
      import c.universe._
      val companionSymbol = tpe.typeSymbol.companionSymbol

      if (companionSymbol == NoSymbol) {
        val clsSymbol = tpe.typeSymbol.asClass
        val msg = "[error] The companion symbol could not be determined for " +
          s"[[${clsSymbol.name}]]. This may be due to a bug in scalac (SI-7567) " +
          "that arises when a case class within a function is pickled. As a " +
          "workaround, move the declaration to the module-level."
        Console.err.println(msg)
        c.abort(c.enclosingPosition, msg) /* TODO Does not show message. */
      }

      val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
      val pre = tpe.asInstanceOf[symTab.Type].prefix.asInstanceOf[Type]
      c.universe.treeBuild.mkAttributedRef(pre, companionSymbol)
    }
    // Should use blackbox.Context in 2.11, doing this for 2.10 compatibility
    def FinalRepr[T: c.WeakTypeTag](c: MacroContext.Context) = c.Expr[PPrint[T]] {
      import c.universe._

      val tpe = c.weakTypeOf[T]

      util.Try(tpe.typeSymbol.asClass) match {

        case util.Success(f) if f.isCaseClass && !f.isModuleClass =>

          val constructor = tpe.member(newTermName("<init>"))

          val companion = companionTree(c)(tpe)

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
            def get = q"$companion.unapply(t).get"
            arity match{
              case 0 => q"()"
              case 1 => q"Tuple1($get)"
              case n => q"$companion.unapply(t).get"
            }
          }
          // We're fleshing this out a lot more than necessary to help
          // scalac along with its implicit search, otherwise it gets
          // confused and explodes
          val res = q"""
            new ammonite.pprint.PPrint[$tpe](
              ammonite.pprint.Internals.fromUnpacker[$tpe](_.productPrefix){
                (t: $tpe, cfg: ammonite.pprint.Config) =>
                  ammonite.pprint
                          .Unpacker
                          .$tupleName[..$paramTypes]
                          .apply($thingy, cfg)
              },
              implicitly[ammonite.pprint.Config]
            )
          """
//          println(res)
          res
        case _ =>
          q"""new ammonite.pprint.PPrint[$tpe](
            ammonite.pprint.PPrinter.Literal,
            implicitly[ammonite.pprint.Config]
          )"""
      }
    }
  }

}
