package ammonite.main
import java.nio.file.NoSuchFileException

import ammonite.interp.api.AmmoniteExit
import ammonite.util.Util.CodeSource
import ammonite.util.{Name, Res, Util}
import fastparse.internal.Util.literalize

/**
  * Logic around using Ammonite as a script-runner; invoking scripts via the
  * macro-generated [[Router]], and pretty-printing any output or error messages
  */
object Scripts {
  def runScript(wd: os.Path,
                path: os.Path,
                interp: ammonite.interp.Interpreter,
                scriptArgs: Seq[String] = Nil) = {
    interp.watch(path)
    val (pkg, wrapper) = Util.pathToPackageWrapper(Seq(), path relativeTo wd)

    for{
      scriptTxt <- try Res.Success(Util.normalizeNewlines(os.read(path))) catch{
        case e: NoSuchFileException => Res.Failure("Script file not found: " + path)
      }

      processed <- interp.processModule(
        scriptTxt,
        CodeSource(wrapper, pkg, Seq(Name("ammonite"), Name("$file")), Some(path)),
        autoImport = true,
        // Not sure why we need to wrap this in a separate `$routes` object,
        // but if we don't do it for some reason the `generateRoutes` macro
        // does not see the annotations on the methods of the outer-wrapper.
        // It can inspect the type and its methods fine, it's just the
        // `methodsymbol.annotations` ends up being empty.
        extraCode = Util.normalizeNewlines(
          s"""
          |val $$routesOuter = this
          |object $$routes
          |extends scala.Function0[mainargs.ParserForMethods[$$routesOuter.type]]{
          |  def apply() = mainargs.ParserForMethods[$$routesOuter.type]($$routesOuter)
          |}
          """.stripMargin
        ),
        hardcoded = true
      )

      routeClsName <- processed.blockInfo.lastOption match{
        case Some(meta) => Res.Success(meta.id.wrapperPath)
        case None => Res.Skip
      }

      scriptMains = interp.scriptCodeWrapper match{
        case ammonite.compiler.CodeWrapper =>
          Some(
            interp
              .evalClassloader
              .loadClass(routeClsName + "$$routes$")
              .getField("MODULE$")
              .get(null)
              .asInstanceOf[() => mainargs.ParserForMethods[Any]]
              .apply()
          )

        case ammonite.compiler.CodeClassWrapper =>
          val outer = interp
            .evalClassloader
            .loadClass(routeClsName)
            .getMethod("instance")
            .invoke(null)

          Some(
            outer.getClass.getMethod("$routes").invoke(outer)
              .asInstanceOf[() => mainargs.ParserForMethods[Any]]
              .apply()
          )
        case _ => None
      }

      res <- Util.withContextClassloader(interp.evalClassloader){
        scriptMains match {
          // If there are no @main methods, there's nothing to do
          case Some(parser) if parser.mains.value.isEmpty =>
            if (scriptArgs.isEmpty) Res.Success(())
            else Res.Failure(
              "Script " + path.last +
              " does not take arguments: " + scriptArgs.map(literalize(_)).mkString(" ")
            )

          // If there's one @main method, we run it with all args
          case Some(parser) =>
            if (scriptArgs.take(1) == Seq("--help")){
              Res.Success(
                new Object{
                  override def toString() = parser.helpText(
                    totalWidth = 100,
                    docsOnNewLine = false
                  )
                }
              )
            }else mainargs.Invoker.runMains(
              parser.mains,
              scriptArgs,
              allowPositional = true,
              allowRepeats = false
            ) match{
              case Left(earlyError) =>
                Res.Failure(mainargs.Renderer.renderEarlyError(earlyError))
              case Right((mainData, result)) =>
                result match{
                  case mainargs.Result.Success(x) => Res.Success(x)
                  case mainargs.Result.Failure.Exception(x: AmmoniteExit) => Res.Success(x.value)
                  case mainargs.Result.Failure.Exception(x) => Res.Exception(x, "")
                  case res: mainargs.Result.Failure =>
                    Res.Failure(
                      mainargs.Renderer.renderResult(
                        mainData,
                        res,
                        totalWidth = 100,
                        printHelpOnError = true,
                        docsOnNewLine = false,
                        customName = None,
                        customDoc = None
                      )
                    )
                }
            }
        }
      }
    } yield res
  }
}
