package ammonite.frontend

import pprint.PPrint

import scala.reflect.runtime.universe._
import acyclic.file


trait DefaultReplAPI extends FullReplAPI {

  def help =
    """Welcome to the Ammonite Scala REPL! Enter a Scala expression and it will be evaluated.
      |All your standard Bash hotkeys should work for navigating around or editing the line
      |being entered, as well as some GUI hotkeys like alt-shift-left/right to select words
      |to replace. Hit <tab> to autocomplete possible names.
      |
      |For a list of REPL built-ins and configuration, use `repl.<tab>`. For a more detailed
      |description of how to use the REPL, check out https://lihaoyi.github.io/Ammonite
    """.stripMargin.trim
  object Internal extends Internal{
    def combinePrints(iters: Iterator[String]*) = {
      iters.toIterator
           .filter(_.nonEmpty)
           .flatMap(Iterator("\n") ++ _)
           .drop(1)
    }

    def print[T: pprint.TPrint: WeakTypeTag, V: PPrint](value: => T,
                                                 value2: => V,
                                                 ident: String,
                                                 custom: Option[String])
                                                (implicit cfg: pprint.Config,
                                                 tcolors: pprint.TPrintColors) = {
      if (typeOf[T] =:= typeOf[Unit]) Iterator()
      else {
        val implicitPPrint = implicitly[PPrint[V]]
        val rhs = custom match {
          case None => implicitPPrint.render(value2, cfg)
          case Some(s) => Iterator(cfg.colors.literalColor(s).render)
        }
        Iterator(
          colors().ident()(ident).render, ": ",
          implicitly[pprint.TPrint[T]].render(tcolors), " = "
        ) ++ rhs
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
