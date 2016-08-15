package ammonite.runtime
import acyclic.file
class APIHolder[T >: Null <: AnyRef] {
  var value0: T = null
  implicit lazy val value = value0
}

object APIHolder{
  def initBridge[T >: Null <: AnyRef](classloader: SpecialClassLoader,
                    name: String,
                    t: T) = {
    classloader.findClassPublic(name + "$")
    classloader.findClassPublic(name)
      .getDeclaredMethods
      .find(_.getName == "value0_$eq")
      .get
      .invoke(null, t)
  }
}

