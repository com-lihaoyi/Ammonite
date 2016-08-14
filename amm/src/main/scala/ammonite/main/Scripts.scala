package ammonite.main
import acyclic.file
import ammonite.interp.ImportHook
import ammonite.main.Router.{ArgSig, EntryPoint}
import ammonite.ops._
import ammonite.util.Name.backtickWrap
import ammonite.util.{Name, Res, Util}
import fastparse.Utils._

/**
  * Logic around using Ammonite as a script-runner; invoking scripts via the
  * macro-generated [[Router]], and pretty-printing any output or error messages
  */
object Scripts {
  def runScript(wd: Path,
                path: Path,
                interp: ammonite.interp.Interpreter,
                args: Seq[String],
                kwargs: Seq[(String, String)]) = {
    val (pkg, wrapper) = Util.pathToPackageWrapper(path, wd)
    for{
      (imports, wrapperHashes) <- interp.processModule(
        ImportHook.Source.File(path),
        Util.normalizeNewlines(read(path)),
        wrapper,
        pkg,
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
        )
      )

      routeClsName = wrapperHashes.last._1

      routesCls =
        interp
          .eval
          .frames
          .head
          .classloader
          .loadClass(routeClsName + "$$routes$")

      scriptMains =
        routesCls
            .getField("MODULE$")
            .get(null)
            .asInstanceOf[() => Seq[Router.EntryPoint]]
            .apply()

      res <- scriptMains match {
        // If there are no @main methods, there's nothing to do
        case Seq() => Res.Success(imports)
        // If there's one @main method, we run it with all args
        case Seq(main) => runMainMethod(main, args, kwargs).getOrElse(Res.Success(imports))
        // If there are multiple @main methods, we use the first arg to decide
        // which method to run, and pass the rest to that main method
        case mainMethods =>
          val suffix = formatMainMethods(mainMethods)
          args match{
            case Seq() =>
              Res.Failure(
                None,
                s"Need to specify a main method to call when running " + path.last + suffix
              )
            case Seq(head, tail @ _*) =>
              mainMethods.find(_.name == head) match{
                case None =>
                  Res.Failure(
                    None,
                    s"Unable to find method: " + backtickWrap(head) + suffix
                  )
                case Some(main) =>
                  runMainMethod(main, tail, kwargs).getOrElse(Res.Success(imports))
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
           |Available main methods:
           |
           |${methods.mkString(Util.newLine)}""".stripMargin
      )
    }
  }
  def runMainMethod(mainMethod: Router.EntryPoint,
                    args: Seq[String],
                    kwargs: Seq[(String, String)]): Option[Res.Failing] = {

    def expectedMsg = {
      val commaSeparated =
        mainMethod.argSignatures
          .map(renderArg)
          .mkString(", ")
      val details = mainMethodDetails(mainMethod)
      "(" + commaSeparated + ")" + details
    }

    mainMethod.invoke(args, kwargs) match{
      case Router.Result.Success(x) => None
      case Router.Result.Error.Exception(x) => Some(Res.Exception(x, ""))
      case Router.Result.Error.TooManyArguments(x) =>
        Some(Res.Failure(
          None,
          Util.normalizeNewlines(
            s"""Too many args were passed to this script: ${x.map(literalize(_)).mkString(", ")}
                |expected arguments: $expectedMsg""".stripMargin
          )

        ))
      case Router.Result.Error.RedundantArguments(x) =>
        Some(Res.Failure(
          None,
          Util.normalizeNewlines(
            s"""Redundant values were passed for arguments: ${x.map(literalize(_)).mkString(", ")}
                |expected arguments: $expectedMsg""".stripMargin
          )
        ))
      case Router.Result.Error.InvalidArguments(x) =>
        Some(Res.Failure(
          None,
          "The following arguments failed to be parsed:" + Util.newLine +
            x.map{
              case Router.Result.ParamError.Missing(p) =>
                s"(${renderArg(p)}) was missing"
              case Router.Result.ParamError.Invalid(p, v, ex) =>
                s"(${renderArg(p)}) failed to parse input ${literalize(v)} with $ex"
              case Router.Result.ParamError.DefaultFailed(p, ex) =>
                s"(${renderArg(p)})'s default value failed to evaluate with $ex"
            }.mkString(Util.newLine) + Util.newLine + s"expected arguments: $expectedMsg"
        ))
    }
  }

  def renderArg(arg: ArgSig) = backtickWrap(arg.name) + ": " + arg.typeString


  def mainMethodDetails(ep: EntryPoint) = {
    ep.argSignatures.collect{
      case ArgSig(name, tpe, Some(doc), default) =>
        Util.newLine + name + " // " + doc
    }.mkString
  }

  /**
    * Additional [[scopt.Read]] instance to teach it how to read Ammonite paths
    */
  implicit def pathScoptRead: scopt.Read[Path] = scopt.Read.stringRead.map(Path(_, cwd))

}
