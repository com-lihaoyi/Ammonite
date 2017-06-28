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
      // This type check was originally written as just typeOf[T] =:= typeOf[Unit].
      // However, due to a bug in Scala's reflection when applied to certain
      // class annotations in Hadoop jars, the type check would consistently
      // throw an exception.
      //
      // The solution is to catch exceptions thrown by the typeOf check and fallback
      // to checking the value against Unit's boxed form.
      //
      // Why not just check the value? Because that would force evaluation of `lazy val`'s
      // which breaks the ammonite.session.EvaluatorTests(lazyvals) test.
      //
      // See https://issues.scala-lang.org/browse/SI-10129 for additional details.
      val isUnit = try {
        classTagT == classTag[Unit]
      } catch {
        case _: Throwable => value == scala.runtime.BoxedUnit.UNIT
      }

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

