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
          |extends scala.Function0[scala.Seq[ammonite.main.Router.EntryPoint[$$routesOuter.type]]]{
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

      mainCls =
        interp
          .evalClassloader
          .loadClass(processed.blockInfo.last.id.wrapperPath + "$")

      scriptMains = interp.scriptCodeWrapper match{
        case ammonite.interp.CodeWrapper =>
          Some(
            interp
              .evalClassloader
              .loadClass(routeClsName + "$$routes$")
              .getField("MODULE$")
              .get(null)
              .asInstanceOf[() => mainargs.ParserForMethods[Any]]
              .apply()
          )

        case ammonite.interp.CodeClassWrapper =>
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
          case None =>
            if (scriptArgs.isEmpty) Res.Success(())
            else Res.Failure(
              "Script " + path.last +
              " does not take arguments: " + scriptArgs.mkString(" ")
            )

          // If there's one @main method, we run it with all args
          case Some(parser) =>
            mainargs.Invoker.runMains(
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
                        docsOnNewLine = false
                      )
                    )
                }
            }
        }
      }
    } yield res
  }

//  def runMainMethod[T](base: T,
//                       mainMethod: Router.EntryPoint[T],
//                       scriptArgs: Seq[(String, Option[String])]): Res[Any] = {
//    val leftColWidth = getLeftColWidth(mainMethod.argSignatures)
//
//    def expectedMsg = formatMainMethodSignature(base: T, mainMethod, 0, leftColWidth)
//
//    def pluralize(s: String, n: Int) = {
//      if (n == 1) s else s + "s"
//    }
//
//    mainMethod.invoke(base, scriptArgs) match{
//      case Router.Result.Success(x) => Res.Success(x)
//      case Router.Result.Error.Exception(x: AmmoniteExit) => Res.Success(x.value)
//      case Router.Result.Error.Exception(x) => Res.Exception(x, "")
//      case Router.Result.Error.MismatchedArguments(missing, unknown, duplicate, incomplete) =>
//        val missingStr =
//          if (missing.isEmpty) ""
//          else {
//            val chunks =
//              for (x <- missing)
//              yield "--" + x.name + ": " + x.typeString
//
//            val argumentsStr = pluralize("argument", chunks.length)
//            s"Missing $argumentsStr: (${chunks.mkString(", ")})" + Util.newLine
//          }
//
//
//        val unknownStr =
//          if (unknown.isEmpty) ""
//          else {
//            val argumentsStr = pluralize("argument", unknown.length)
//            s"Unknown $argumentsStr: " + unknown.map(literalize(_)).mkString(" ") + Util.newLine
//          }
//
//        val duplicateStr =
//          if (duplicate.isEmpty) ""
//          else {
//            val lines =
//              for ((sig, options) <- duplicate)
//              yield {
//                s"Duplicate arguments for (--${sig.name}: ${sig.typeString}): " +
//                options.map(literalize(_)).mkString(" ") + Util.newLine
//              }
//
//            lines.mkString
//
//          }
//        val incompleteStr = incomplete match{
//          case None => ""
//          case Some(sig) =>
//            s"Option (--${sig.name}: ${sig.typeString}) is missing a corresponding value" +
//            Util.newLine
//
//          }
//
//        Res.Failure(
//          Util.normalizeNewlines(
//            s"""$missingStr$unknownStr$duplicateStr$incompleteStr
//               |Arguments provided did not match expected signature:
//               |
//               |$expectedMsg
//               |""".stripMargin
//          )
//        )
//
//      case Router.Result.Error.InvalidArguments(x) =>
//        val argumentsStr = pluralize("argument", x.length)
//        val thingies = x.map{
//          case Router.Result.ParamError.Invalid(p, v, ex) =>
//            val literalV = literalize(v)
//            val rendered = {renderArgShort(p)}
//            s"$rendered: ${p.typeString} = $literalV failed to parse with $ex"
//          case Router.Result.ParamError.DefaultFailed(p, ex) =>
//            s"${renderArgShort(p)}'s default value failed to evaluate with $ex"
//        }
//
//        Res.Failure(
//          Util.normalizeNewlines(
//            s"""The following $argumentsStr failed to parse:
//              |
//              |${thingies.mkString(Util.newLine)}
//              |
//              |expected signature:
//              |
//              |$expectedMsg
//            """.stripMargin
//          )
//        )
//    }
//  }


}
