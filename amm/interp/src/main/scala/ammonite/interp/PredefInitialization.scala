package ammonite.interp


import ammonite.runtime.{APIHolder, SpecialClassLoader, Storage}
import ammonite.util.ScriptOutput.Metadata
import ammonite.util.{Imports, Name, PredefInfo, Res}
import ammonite.util.Util.CodeSource

/**
  * The logic around executing an [[Interpreter]]'s predef during
  * initialization
  */
object PredefInitialization {
  def apply(bridges: Seq[(String, String, AnyRef, () => Unit)],
            interpApi: InterpAPI,
            evalClassloader: SpecialClassLoader,
            storage: Storage,
            customPredefs: Seq[PredefInfo],
            processModule: (String, CodeSource, Boolean) => Res[Metadata],
            addImports: Imports => Unit) = {

    for ((name, shortName, bridge, cb) <- bridges ){
      APIHolder.initBridge(evalClassloader, name, bridge)
    }
    // import ammonite.repl.ReplBridge.{value => repl}
    // import ammonite.runtime.InterpBridge.{value => interp}
    val bridgePredefs =
      for ((name, shortName, bridge, cb) <- bridges)
      yield PredefInfo(
        Name(s"${shortName}Bridge"),
        s"import $name.{value => $shortName}",
        true,
        None
      )

    val predefs = {
      val (sharedPredefContent, sharedPredefPath) = storage.loadSharedPredef
      val (predefContent, predefPath) = storage.loadPredef
      bridgePredefs ++ customPredefs ++ Seq(
        PredefInfo(Name("UserSharedPredef"), sharedPredefContent, false, sharedPredefPath),
        PredefInfo(Name("UserPredef"), predefContent, false, predefPath)
      )
    }

    predefs.filter(_.code.nonEmpty)


    for (predefInfo <- predefs if predefInfo.code.nonEmpty) yield {
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
        case Res.Success(processed) => addImports(processed.blockInfo.last.finalImports)

        case Res.Failure(ex, msg) =>
          ex match{
            case Some(e) => throw new RuntimeException("Error during Predef: " + msg, e)
            case None => throw new RuntimeException("Error during Predef: " + msg)
          }

        case Res.Exception(ex, msg) =>
          throw new RuntimeException("Error during Predef: " + msg, ex)

        case _ => ???
      }
    }
  }
}
