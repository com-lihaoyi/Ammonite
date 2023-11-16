package ammonite.interp.api

class APIHolder[T >: Null <: AnyRef] {
  var value0: T = null
  implicit final lazy val value: T = value0
}
