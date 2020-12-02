package ammonite.interp


import ammonite.compiler.iface.{CodeSource, Imports}
import ammonite.interp.api.InterpAPI
import ammonite.runtime.{SpecialClassLoader, Storage}
import ammonite.util.InterfaceExtensions._
import ammonite.util.ScriptOutput.Metadata
import ammonite.util.{ImportData, Name, PredefInfo, Res}

/**
  * The logic around executing an [[Interpreter]]'s predef during
  * initialization
  */
object PredefInitialization {
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
  def initBridges(bridges: Seq[(String, String)]): Imports = {

    val allImports =
      for ((name, shortName) <- bridges)
        yield new Imports(
          Array(new Imports.Data(
            "value",
            shortName,
            // FIXME Not sure special chars / backticked things in name are fine here
            "_root_" +: name.stripPrefix("_root_.").split('.').toArray,
            "Term"
          ))
        )

    allImports.foldLeft(new Imports)(_ ++ _)
  }
  def initBridges(bridges: Seq[(String, String, AnyRef)],
                  evalClassloader: SpecialClassLoader): Imports = {

    for ((name, shortName, bridge) <- bridges)
      initBridge(evalClassloader, name, bridge)

    initBridges(bridges.map { case (name, shortName, _) => (name, shortName) })
  }
  def apply(interpApi: InterpAPI,
            storage: Storage,
            basePredefs: Seq[PredefInfo],
            customPredefs: Seq[PredefInfo],
            processModule: (String, CodeSource, Boolean) => Res[Metadata],
            addImports: Imports => Unit,
            watch: os.Path => Unit): Res[_] = {

    val predefs = {
      basePredefs ++
      storage.loadPredef.map{
        case (code, path) =>
          PredefInfo(Name(path.last.stripSuffix(".sc")), code, false, Some(path))
      } ++
      customPredefs
    }


    Res.fold((), predefs){(_, predefInfo) =>
      predefInfo.path.foreach(watch)
      if (predefInfo.code.isEmpty) Res.Success(())
      else {
        processModule(
          predefInfo.code,
          new CodeSource(
            predefInfo.name.raw,
            Array(),
            Array("ammonite", "predef"),
            predefInfo.path.map(_.toNIO).orNull
          ),
          predefInfo.hardcoded
        ) match{
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
