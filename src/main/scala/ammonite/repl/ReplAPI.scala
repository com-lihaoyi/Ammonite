package ammonite.repl

import ammonite.util.Ref
import ammonite.util.Util.newLine
import pprint.{Config, PPrint}

import scala.reflect.runtime.universe._
import ammonite.runtime.{APIHolder, History, ReplExit}

trait ReplAPI {

  /**
    * Exit the Ammonite REPL. You can also use Ctrl-D to exit
    */
  def exit = throw ReplExit(())

  /**
    * Exit the Ammonite REPL. You can also use Ctrl-D to exit
    */
  def exit(value: Any) = throw ReplExit(value)

  /**
    * History of commands that have been entered into the shell, including
    * previous sessions
    */
  def fullHistory: History

  /**
    * History of commands that have been entered into the shell during the
    * current session
    */
  def history: History

  /**
    * Get the `Type` object of [[T]]. Useful for finding
    * what its methods are and what you can do with it
    */
  def typeOf[T: WeakTypeTag]: Type

  /**
    * Get the `Type` object representing the type of `t`. Useful
    * for finding what its methods are and what you can do with it
    *
    */
  def typeOf[T: WeakTypeTag](t: => T): Type

  /**
    * Throw away the current scala.tools.nsc.Global and get a new one
    */
  def newCompiler(): Unit

  /**
    * Access the compiler to do crazy things if you really want to!
    */
  def compiler: scala.tools.nsc.Global

  /**
    * Show all the imports that are used to execute commands going forward
    */
  def imports: String

  /**
    * Controls how things are pretty-printed in the REPL. Feel free
    * to shadow this with your own definition to change how things look
    */
  implicit val pprintConfig: Ref[pprint.Config]

  implicit def derefPPrint(implicit t: Ref[pprint.Config]): pprint.Config = t()

  /**
    * Lets you configure the pretty-printing of a value. By default, it simply
    * disables truncation and prints the entire thing, but you can set other
    * parameters as well if you want.
    */
  def show[T: PPrint](implicit cfg: Config): T => Unit
  def show[T: PPrint](t: T,
                      width: Integer = 0,
                      height: Integer = null,
                      indent: Integer = null,
                      colors: pprint.Colors = null)(implicit cfg: Config = Config.Defaults.PPrintConfig): Unit

}

// End of ReplAPI
/**
  * Things that are part of the ReplAPI that aren't really "public"
  */
abstract class FullReplAPI extends ReplAPI {

  val Internal: Internal
  trait Internal {
    def combinePrints(iters: Iterator[String]*): Iterator[String]

    /**
      * Kind of an odd signature, splitting out [[T]] and [[V]]. This is
      * seemingly useless but necessary because when you add both [[pprint.TPrint]]
      * and [[PPrint]] context bounds to the same type, Scala's type inference
      * gets confused and does the wrong thing
      */
    def print[T: pprint.TPrint: WeakTypeTag, V: PPrint](
        value: => T,
        value2: => V,
        ident: String,
        custom: Option[String])(implicit cfg: Config, tcolors: pprint.TPrintColors): Iterator[String]

    def printDef(definitionLabel: String, ident: String): Iterator[String]
    def printImport(imported: String): Iterator[String]
  }
  def typeOf[T: WeakTypeTag] = scala.reflect.runtime.universe.weakTypeOf[T]
  def typeOf[T: WeakTypeTag](t: => T) =
    scala.reflect.runtime.universe.weakTypeOf[T]
}

trait DefaultReplAPI extends FullReplAPI {

  object Internal extends Internal {
    def combinePrints(iters: Iterator[String]*) = {
      iters.toIterator.filter(_.nonEmpty).flatMap(Iterator(newLine) ++ _).drop(1)
    }

    def print[T: pprint.TPrint: WeakTypeTag, V: PPrint](
        value: => T,
        value2: => V,
        ident: String,
        custom: Option[String])(implicit cfg: pprint.Config, tcolors: pprint.TPrintColors) = {
      if (typeOf[T] =:= typeOf[Unit]) Iterator()
      else {
        val implicitPPrint = implicitly[PPrint[V]]
        val rhs = custom match {
          case None => implicitPPrint.render(value2, cfg)
          case Some(s) => Iterator(cfg.colors.literalColor(s).render)
        }
        Iterator(
          ident,
          ": ",
          implicitly[pprint.TPrint[T]].render(tcolors),
          " = "
        ) ++ rhs
      }
    }
    def printDef(definitionLabel: String, ident: String) = {
      Iterator(
        "defined ",
        definitionLabel,
        " ",
        ident
      )
    }
    def printImport(imported: String) = {
      Iterator("import ", imported)
    }
  }
}

object ReplBridge extends APIHolder[FullReplAPI]