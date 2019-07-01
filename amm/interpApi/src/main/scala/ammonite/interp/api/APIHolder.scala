package ammonite.interp.api

class APIHolder[T >: Null <: AnyRef] {
  var value0: T = null
  implicit lazy val value = value0
}
