package ammonite.compiler

import ammonite.interp.api.InterpAPI

object Extensions {

  implicit class CompilerInterAPIExtensions(private val self: InterpAPI) extends AnyVal

}
