package ammonite

import scala.collection.Seq

package object ops extends Extensions with RelPathStuff{
  /**
   * The root of the filesystem
   */
  val root = ops.Path.root

  /**
   * The user's home directory
   */
  val home = Path(System.getProperty("user.home"))

  /**
   * The current working directory for this process.
   */
  def processWorkingDir = ops.Path(new java.io.File(""))

  implicit class Transformable1(p: java.nio.file.Path){
    def amm = {
      val s = p.toString

      if (s.startsWith("/")) ops.Path(s)
      else ops.RelPath(s)
    }
  }

  /**
   * Extractor to let you easily pattern match on [[ops.Path]]s
   */
  object /{

    def unapply[T <: BasePath[T]](p: T): Option[(T, String)] = {
      if (p.segments.length > 0)
        Some((p / up, p.last))
      else None
    }
  }


  implicit def fileData(p: Path) = stat.full(p)
}
