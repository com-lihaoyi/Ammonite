package ammonite

import ammonite.ops.{Path, up}

package object util {
  type ClassFiles = Vector[(String, Array[Byte])]

  type ImportMapping = Seq[(String, Option[String])]
  case class ImportTree(prefix: Seq[String],
                        mappings: Option[ImportMapping],
                        start: Int,
                        end: Int)

  def pathToPackageWrapper(path: Path, wd: Path): (Seq[Name], Name) = {
    val pkg = {
      val base = Seq("$file")
      val relPath = (path/up).relativeTo(wd)
      val ups = Seq.fill(relPath.ups)("..")
      val rest = relPath.segments
      (base ++ ups ++ rest).map(Name(_))
    }
    val wrapper = path.last.take(path.last.lastIndexOf('.'))
    (pkg, Name(wrapper))
  }

  val scalaBinaryVersion =
    scala.util.Properties
      .versionString
      .stripPrefix("version ")
      .split('.')
      .take(2)
      .mkString(".")
}
