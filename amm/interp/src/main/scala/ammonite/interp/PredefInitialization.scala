package ammonite.interp


import ammonite.interp.api.{APIHolder, InterpAPI}
import ammonite.runtime.{SpecialClassLoader, Storage}
import ammonite.util.ScriptOutput.Metadata
import ammonite.util.{Imports, Name, PredefInfo, Res}
import ammonite.util.Util.CodeSource

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
  def apply(bridges: Seq[(String, String, AnyRef)],
            interpApi: InterpAPI,
            evalClassloader: SpecialClassLoader,
            storage: Storage,
            basePredefs: Seq[PredefInfo],
            customPredefs: Seq[PredefInfo],
            processModule: (String, CodeSource, Boolean) => Res[Metadata],
            addImports: Imports => Unit,
            watch: os.Path => Unit): Res[_] = {

    for ((name, shortName, bridge) <- bridges ){
      initBridge(evalClassloader, name, bridge)
    }

    // import ammonite.repl.api.ReplBridge.{value => repl}
    // import ammonite.interp.api.InterpBridge.{value => interp}
    val bridgePredefs =
      for ((name, shortName, bridge) <- bridges)
      yield PredefInfo(
        Name(s"${shortName}Bridge"),
        s"import $name.{value => $shortName}",
        true,
        None
      )

    val predefs = {
      bridgePredefs ++
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
          CodeSource(
            predefInfo.name,
            Seq(),
            Seq(Name("ammonite"), Name("predef")),
            predefInfo.path
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
