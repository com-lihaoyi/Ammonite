package ammonite


package object ops extends ShelloutRoots(Nil, false) with Extensions with RelPathStuff{
  implicit val postfixOps = scala.language.postfixOps

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
  lazy val cwd = ops.Path(new java.io.File(""))

  object ImplicitWd{
    implicit lazy val implicitCwd = ops.cwd
  }

  implicit class Transformable1(p: java.nio.file.Path){
    def amm = {
      import collection.JavaConversions._
      if (p.toAbsolutePath.iterator().size == p.iterator().size) ops.Path(p)
      else ops.RelPath(p)
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

  implicit class PathCallable[T <% Path](p: T) extends ShelloutRoots(List((p: Path).toString), true)
}
