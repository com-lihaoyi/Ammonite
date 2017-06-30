package ammonite.repl

import ammonite.util.{Bind, _}
import ammonite.util.Util.newLine

import scala.reflect.runtime.universe._
import ammonite.runtime.APIHolder

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.classTag

trait FullReplAPI extends ReplAPI{

  def typeOf[T: WeakTypeTag] = scala.reflect.runtime.universe.weakTypeOf[T]
  def typeOf[T: WeakTypeTag](t: => T) = scala.reflect.runtime.universe.weakTypeOf[T]

  protected val colors: Ref[Colors]

  def help =
    """Welcome to the Ammonite Scala REPL! Enter a Scala expression and it will be evaluated.
      |All your standard Bash hotkeys should work for navigating around or editing the line
      |being entered, as well as some GUI hotkeys like alt-shift-left/right to select words
      |to replace. Hit <tab> to autocomplete possible names.
      |
      |For a list of REPL built-ins and configuration, use `repl.<tab>`. For a more detailed
      |description of how to use the REPL, check out https://lihaoyi.github.io/Ammonite
    """.stripMargin.trim


  protected[this] def replArgs0: IndexedSeq[Bind[_]]
  /**
    * This stuff is used for the REPL-generated code that prints things;
    * shouldn't really be used by users, but needs to be public and accessible
    */
  object Internal {
    def replArgs: IndexedSeq[Bind[_]] = replArgs0
    def combinePrints(iters: Iterator[String]*) = {
      iters.toIterator
           .filter(_.nonEmpty)
           .flatMap(Iterator(newLine) ++ _)
           .drop(1)
    }

    def print[T: pprint.TPrint](value: => T,
                                ident: String,
                                custom: Option[String])
                               (implicit tcolors: pprint.TPrintColors,
                                classTagT: ClassTag[T] = null) = {
      // Here we use ClassTag to detect if T is an Unit.
      // The default value null suppresses the compilation error when T is a singleton type,
      // which can't provide a ClassTag.
      //
      // We don't use `asUnit: T =:= Unit = null` because that approach does not work
      // when T is Nothing.
      // See https://github.com/scala/bug/issues/10393 for further information.
      //
      // We don't use WeakTypeTag or TypeTag because those type classes are too heavy-weight,
      // as Scalac will generate a huge amount of code for creating a TypeTag for refinement types.
      // See https://github.com/lihaoyi/Ammonite/issues/649 for further information.
      val isUnit = classTagT == classTag[Unit]

      if (isUnit) Iterator()
      else {

        // Pre-compute how many lines and how many columns the prefix of the
        // printed output takes, so we can feed that information into the
        // pretty-printing of the main body
        val prefix = new pprint.Truncated(
          Iterator(
            colors().ident()(ident).render, ": ",
            implicitly[pprint.TPrint[T]].render(tcolors), " = "
          ),
          pprinter().defaultWidth,
          pprinter().defaultHeight
        )
        val output = mutable.Buffer.empty[fansi.Str]

        prefix.foreach(output +=)

        val rhs = custom match {
          case None =>
            pprinter().tokenize(
              value,
              height = pprinter().defaultHeight - prefix.completedLineCount,
              initialOffset = prefix.lastLineLength
            ).toStream
          case Some(s) => Seq(pprinter().colorLiteral(s))
        }

        output.iterator.map(_.render) ++ rhs.map(_.render)
      }
    }
    def printDef(definitionLabel: String, ident: String) = {
      Iterator(
        "defined ", colors().`type`()(definitionLabel).render, " ",
        colors().ident()(ident).render
      )
    }
    def printImport(imported: String) = {
      Iterator(colors().`type`()("import ").render, colors().ident()(imported).render)
    }
  }
}

object ReplBridge extends APIHolder[FullReplAPI]

