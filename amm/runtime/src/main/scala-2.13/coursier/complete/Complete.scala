package coursier.complete

// remove once we can switch to scala 2.13.0-RC1, and to the same coursier
// version as the rest of Ammonite for it

class Complete {
  def withScalaVersion(ver: String): Complete = this
  def withInput(s: String): Complete = this
  def complete(): coursier.util.Task[(Int, Seq[String])] =
    coursier.util.Task.point(0 -> Nil)
}

object Complete {
  def apply(cache: coursier.cache.Cache[coursier.util.Task]): Complete =
    new Complete
}
