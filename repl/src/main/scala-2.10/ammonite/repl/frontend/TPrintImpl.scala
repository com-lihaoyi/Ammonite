import ammonite.repl.frontend.TPrint
import pprint.Config

import scala.reflect.runtime.universe._

trait TPrintLowPri{
  implicit def default[T: TypeTag]: TPrint[T] = new TPrint[T] {
    def render(implicit cfg: Config): String = typeOf[T].toString
  }
}