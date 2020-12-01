package ammonite

import ammonite.interp.api.APIHolder
import ammonite.repl.api.ReplAPI
import ammonite.repl.ReplExtras.{PrintHook, ReplAPIExtensions}

trait TestReplApi {
  def message: String
}

object TestReplBridge extends APIHolder[TestReplApi]

case class Nope(n: Int)

object Nope {
  def initPrint(implicit replApi: ReplAPI): Unit =
    replApi.addPrintHook(
      new PrintHook {
        def print[T](value: => T,
                      ident: String,
                      custom: Option[String])
                     (implicit tprint: pprint.TPrint[T],
                      tcolors: pprint.TPrintColors,
                      api: ReplAPI,
                      classTagT: scala.reflect.ClassTag[T]): Option[Iterator[String]] =
          if (classTagT == scala.reflect.classTag[ammonite.Nope]) Some(Iterator.empty)
          else None
      }
    )
}
