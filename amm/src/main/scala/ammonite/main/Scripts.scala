package ammonite.main
import java.nio.file.NoSuchFileException


import ammonite.main.Router.{ArgSig, EntryPoint}
import ammonite.ops._
import ammonite.runtime.Evaluator.AmmoniteExit
import ammonite.util.Name.backtickWrap
import ammonite.util.Util.CodeSource
import ammonite.util.{Name, Res, Util}
import fastparse.utils.Utils._

/**
  * Logic around using Ammonite as a script-runner; invoking scripts via the
  * macro-generated [[Router]], and pretty-printing any output or error messages
  */
object Scripts {
  def groupArgs(flatArgs: List[String]): Seq[(String, Option[String])] = {
    var keywordTokens = flatArgs
    var scriptArgs = Vector.empty[(String, Option[String])]

    while(keywordTokens.nonEmpty) keywordTokens match{
      case List(head, next, rest@_*) if head.startsWith("--") =>
        scriptArgs = scriptArgs :+ (head.drop(2), Some(next))
        keywordTokens = rest.toList
      case List(head, next, rest@_*) if head.startsWith("-") =>
        scriptArgs = scriptArgs :+ (head.drop(1), Some(next))
        keywordTokens = rest.toList
      case List(head, rest@_*) =>
        scriptArgs = scriptArgs :+ (head, None)
        keywordTokens = rest.toList

    }
    scriptArgs
  }
  def runScript(wd: Path,
                path: Path,
                interp: ammonite.interp.Interpreter,
                scriptArgs: Seq[(String, Option[String])]) = {
    interp.interpApi.watch(path)
    val (pkg, wrapper) = Util.pathToPackageWrapper(Seq(), path relativeTo wd)

    for{
      scriptTxt <- try Res.Success(Util.normalizeNewlines(read(path))) catch{
        case e: NoSuchFileException => Res.Failure(Some(e), "Script file not found: " + path)
      }
      processed <- interp.processModule(
        scriptTxt,
        CodeSource(wrapper, pkg, Seq(Name("$file")), Some(path)),
        autoImport = true,
        // Not sure why we need to wrap this in a separate `$routes` object,
        // but if we don't do it for some reason the `generateRoutes` macro
        // does not see the annotations on the methods of the outer-wrapper.
        // It can inspect the type and its methods fine, it's just the
        // `methodsymbol.annotations` ends up being empty.
        extraCode = Util.normalizeNewlines(
          s"""
          |val $$routesOuter = this
          |object $$routes extends scala.Function0[scala.Seq[ammonite.main.Router.EntryPoint]]{
          |  def apply() = ammonite.main.Router.generateRoutes[$$routesOuter.type]($$routesOuter)
          |}
          """.stripMargin
        ),
        hardcoded = true
      )

      routeClsName <- processed.blockInfo.lastOption match{
        case Some(meta) => Res.Success(meta.id.wrapperPath)
        case None => Res.Skip
      }

      routesCls =
        interp
          .eval
          .evalClassloader
          .loadClass(routeClsName + "$$routes$")

      scriptMains =
        routesCls
            .getField("MODULE$")
            .get(null)
            .asInstanceOf[() => Seq[Router.EntryPoint]]
            .apply()

      res <- interp.eval.withContextClassloader{
        scriptMains match {
          // If there are no @main methods, there's nothing to do
          case Seq() =>
            if (scriptArgs.isEmpty) Res.Success(())
            else {
              val scriptArgString =
                scriptArgs.flatMap{case (a, b) => Seq(a) ++ b}.map(literalize(_))
                          .mkString(" ")

              Res.Failure(
                None,
                "Script " + path.last + " does not take arguments: " + scriptArgString
              )
            }

          // If there's one @main method, we run it with all args
          case Seq(main) => runMainMethod(main, scriptArgs)

          // If there are multiple @main methods, we use the first arg to decide
          // which method to run, and pass the rest to that main method
          case mainMethods =>
            val suffix = formatMainMethods(mainMethods)
            scriptArgs match{
              case Seq() =>
                Res.Failure(
                  None,
                  s"Need to specify a subcommand to call when running " + path.last + suffix
                )
              case Seq((head, Some(_)), tail @ _*) =>
                Res.Failure(
                  None,
                  "To select a subcommand to run, you don't need --s." + Util.newLine +
                  s"Did you mean `${head.drop(2)}` instead of `$head`?"
                )
              case Seq((head, None), tail @ _*) =>
                mainMethods.find(_.name == head) match{
                  case None =>
                    Res.Failure(
                      None,
                      s"Unable to find subcommand: " + backtickWrap(head) + suffix
                    )
                  case Some(main) =>
                    runMainMethod(main, tail)
                }
            }
        }
      }
    } yield res
  }
  def formatMainMethods(mainMethods: Seq[Router.EntryPoint]) = {
    if (mainMethods.isEmpty) ""
    else{
      val methods = for(main <- mainMethods) yield{
        val args = main.argSignatures.map(renderArg).mkString(", ")
        val details = mainMethodDetails(main)
        s"def ${main.name}($args)$details"
      }
      Util.normalizeNewlines(
        s"""
           |
           |Available subcommands:
           |
           |${methods.mkString(Util.newLine)}""".stripMargin
      )
    }
  }
  def runMainMethod(mainMethod: Router.EntryPoint,
                    scriptArgs: Seq[(String, Option[String])]): Res[Any] = {

    def expectedMsg = {
      val commaSeparated =
        mainMethod.argSignatures
          .map(renderArg)
          .mkString(", ")
      val details = mainMethodDetails(mainMethod)
      "def " + mainMethod.name + "(" + commaSeparated + ")" + details
    }

    mainMethod.invoke(scriptArgs) match{
      case Router.Result.Success(x) => Res.Success(x)
      case Router.Result.Error.Exception(x: AmmoniteExit) => Res.Success(x.value)
      case Router.Result.Error.Exception(x) => Res.Exception(x, "")
      case Router.Result.Error.MismatchedArguments(missing, unknown, duplicate) =>
        val missingStr =
          if (missing.isEmpty) ""
          else {
            val chunks =
              for (x <- missing)
              yield x.name + ": " + x.typeString

            s"Missing arguments: (${chunks.mkString(", ")})" + Util.newLine
          }


        val unknownStr =
          if (unknown.isEmpty) ""
          else s"Unknown arguments: " + unknown.map(literalize(_)).mkString(", ") + Util.newLine

        val duplicateStr =
          if (duplicate.isEmpty) ""
          else {
            val lines =
              for ((sig, options) <- duplicate)
              yield {
                s"Duplicate arguments for (${sig.name}: ${sig.typeString}): " +
                options.map(literalize(_)) + Util.newLine
              }

            lines.mkString

          }

        Res.Failure(
          None,
          Util.normalizeNewlines(
            s"""Arguments provided did not match expected signature:
               |$expectedMsg
               |
               |$missingStr$unknownStr$duplicateStr
               |""".stripMargin
          )
        )

      case Router.Result.Error.InvalidArguments(x) =>
        Res.Failure(
          None,
          "The following arguments failed to be parsed:" + Util.newLine +
            x.map{
              case Router.Result.ParamError.Invalid(p, v, ex) =>
                s"(${renderArg(p)}) failed to parse input ${literalize(v)} with $ex"
              case Router.Result.ParamError.DefaultFailed(p, ex) =>
                s"(${renderArg(p)})'s default value failed to evaluate with $ex"
            }.mkString(Util.newLine) + Util.newLine + s"expected signature: $expectedMsg"
        )
    }
  }

  def renderArg(arg: ArgSig) = {
    val suffix = arg.default match{
      case Some(f) => " = " + f()
      case None => ""
    }
    backtickWrap(arg.name) + ": " + arg.typeString + suffix
  }


  def mainMethodDetails(ep: EntryPoint) = {
    ep.argSignatures.collect{
      case ArgSig(name, tpe, Some(doc), default) =>
        Util.newLine + name + " // " + doc
    }.mkString
  }

  /**
    * Additional [[scopt.Read]] instance to teach it how to read Ammonite paths
    */
  implicit def pathScoptRead: scopt.Read[Path] = scopt.Read.stringRead.map(Path(_, pwd))

}
