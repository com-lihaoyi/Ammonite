package ammonite.interp

import ammonite.interp.api.InterpAPI
import ammonite.runtime.{SpecialClassLoader, Storage}
import ammonite.util.ScriptOutput.Metadata
import ammonite.util.{ImportData, Imports, Name, PredefInfo, Res}
import ammonite.util.Util.CodeSource

/**
 * The logic around executing an [[Interpreter]]'s predef during
 * initialization
 */
object PredefInitialization {
  def initBridge[T >: Null <: AnyRef](classloader: SpecialClassLoader, name: String, t: T) = {
    classloader.findClassPublic(name + "$")
    classloader.findClassPublic(name)
      .getDeclaredMethods
      .find(_.getName == "value0_$eq")
      .get
      .invoke(null, t)
  }
  def initBridges(bridges: Seq[(String, String)]): Imports = {

    val allImports =
      for ((name, shortName) <- bridges)
        yield Imports(
          Seq(ImportData(
            Name("value"),
            Name(shortName),
            // FIXME Not sure special chars / backticked things in name are fine here
            Name("_root_") +: name.stripPrefix("_root_.").split('.').map(Name(_)).toSeq,
            ImportData.Term
          ))
        )

    allImports.foldLeft(Imports())(_ ++ _)
  }
  def initBridges(
      bridges: Seq[(String, String, AnyRef)],
      evalClassloader: SpecialClassLoader
  ): Imports = {

    for ((name, shortName, bridge) <- bridges)
      initBridge(evalClassloader, name, bridge)

    initBridges(bridges.map { case (name, shortName, _) => (name, shortName) })
  }
  def apply(
      interpApi: InterpAPI,
      storage: Storage,
      basePredefs: Seq[PredefInfo],
      customPredefs: Seq[PredefInfo],
      processModule: (String, CodeSource, Boolean) => Res[Metadata],
      addImports: Imports => Unit,
      watch: os.Path => Unit
  ): Res[_] = {

    val predefs = {
      basePredefs ++
        storage.loadPredef.map {
          case (code, path) =>
            PredefInfo(Name(path.last.stripSuffix(".sc")), code, false, Some(path))
        } ++
        customPredefs
    }

    Res.fold((), predefs) { (_, predefInfo) =>
      predefInfo.path.foreach(watch)
      if (predefInfo.code.isEmpty) Res.Success(())
      else {
        processModule(
          predefInfo.code,
          CodeSource(
            predefInfo.name,
            Seq(),
            Seq(Name("ammonite"), Name("predef")),
            predefInfo.path
          ),
          predefInfo.hardcoded
        ) match {
          case Res.Skip => Res.Success(())
          case Res.Success(processed) =>
            addImports(processed.blockInfo.last.hookInfo.imports)
            addImports(processed.blockInfo.last.finalImports)
            Res.Success(())

          case x => x.map(_ => ())
        }

      }
    }
  }
}
