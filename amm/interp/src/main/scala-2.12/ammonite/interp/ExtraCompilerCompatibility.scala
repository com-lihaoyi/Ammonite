package ammonite.interp

import scala.tools.nsc.Global

abstract class ExtraCompilerCompatibility {
  def importInfo(g: Global)(t: g.Import) =
    new g.analyzer.ImportInfo(t, 0)
}