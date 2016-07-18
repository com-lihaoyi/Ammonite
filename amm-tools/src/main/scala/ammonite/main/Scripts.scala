package ammonite.main
import acyclic.file
import ammonite.interp.ImportHook
import ammonite.interp.ImportHook.InterpreterInterface
import ammonite.main.Router.{ArgSig, EntryPoint}
import ammonite.ops._
import ammonite.util.Name.backtickWrap
import ammonite.util.{Name, pathToPackageWrapper, Res}

import scala.annotation.switch

/**
  * Logic around using Ammonite as a script-runner; invoking scripts via the
  * macro-generated [[Router]], and pretty-printing any output or error messages
  */
object Scripts {


  /**
    * Convert a string to a C&P-able literal. Basically
    * copied verbatim from the uPickle source code.
    */
  def literalize(s: String, unicode: Boolean = true) = {
    val sb = new StringBuilder
    sb.append('"')
    var i = 0
    val len = s.length
    while (i < len) {
      (s.charAt(i): @switch) match {
        case '"' => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\b' => sb.append("\\b")
        case '\f' => sb.append("\\f")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case c =>
          if (c < ' ' || (c > '~' && unicode)) sb.append("\\u%04x" format c.toInt)
          else sb.append(c)
      }
      i += 1
    }
    sb.append('"')

  }


  def runScript(wd: Path,
                path: Path,
                intp: InterpreterInterface,
                mainMethodName: Option[String],
                args: Seq[String],
                kwargs: Seq[(String, String)]) = {
    val (pkg, wrapper) = pathToPackageWrapper(path, wd)
    for{
      imports <- intp.processModule(
        ImportHook.Source.File(path),
        read(path),
        wrapper,
        pkg,
        autoImport = true
      )
      _ <- {
        intp.reInit()

        val fullName = (pkg :+ wrapper).map(_.backticked).mkString(".")
        intp.processModule(
          ImportHook.Source.File(cwd/"<console>"),
          s"val routes = ammonite.main.Router.generateRoutes[$fullName.type]($fullName)",
          Name("MainRouter"),
          Seq(Name("$sess")),
          autoImport = false
        )
      }
      entryPoints =
        intp
            .eval
            .sess
            .frames
            .head
            .classloader
            .loadClass("$sess.MainRouter")
            .getMethods
            .find(_.getName == "routes")
            .get
            .invoke(null)
            .asInstanceOf[Seq[Router.EntryPoint]]
            .filter(_.name != "$main")

      res <- mainMethodName match {
        case None =>
          entryPoints.find(_.name == "main") match {
            case None => Res.Success(imports)
            case Some(entry) =>
              runEntryPoint(entry, args, kwargs).getOrElse(Res.Success(imports))
          }
        case Some(s) =>
          entryPoints.find(_.name == s) match{
            case None =>
              val suffix =
                if (entryPoints.isEmpty) ""
                else{
                  val methods = for(ep <- entryPoints) yield{
                    val args = ep.argSignatures.map(renderArg).mkString(", ")
                    val details = entryDetails(ep)
                    s"def ${ep.name}($args)$details"
                  }
                  s"""
                     |
                     |Existing methods:
                     |
                     |${methods.mkString("\n\n")}""".stripMargin
                }
              Res.Failure(
                None,
                s"Unable to find method: ${backtickWrap(s)}" + suffix
              )
            case Some(entry) =>
              runEntryPoint(entry, args, kwargs).getOrElse(Res.Success(imports))
          }

      }
    } yield res
  }

  def runEntryPoint(entry: Router.EntryPoint,
                    args: Seq[String],
                    kwargs: Seq[(String, String)]): Option[Res.Failing] = {

    def expectedMsg = {
      val commaSeparated =
        entry.argSignatures
          .map(renderArg)
          .mkString(", ")
      val details = entryDetails(entry)
      "(" + commaSeparated + ")" + details
    }

    entry.invoke(args, kwargs) match{
      case Router.Result.Success(x) => None
      case Router.Result.Error.Exception(x) => Some(Res.Exception(x, ""))
      case Router.Result.Error.TooManyArguments(x) =>
        Some(Res.Failure(
          None,
          s"""Too many args were passed to this script: ${x.map(literalize(_)).mkString(", ")}
              |expected arguments $expectedMsg""".stripMargin

        ))
      case Router.Result.Error.RedundantArguments(x) =>
        Some(Res.Failure(
          None,
          s"""Redundant values were passed for arguments: ${x.map(literalize(_)).mkString(", ")}
              |expected arguments: $expectedMsg""".stripMargin
        ))
      case Router.Result.Error.InvalidArguments(x) =>
        Some(Res.Failure(
          None,
          "The following arguments failed to be parsed:\n" +
            x.map{
              case Router.Result.ParamError.Missing(p) =>
                s"(${renderArg(p)}) was missing"
              case Router.Result.ParamError.Invalid(p, v, ex) =>
                s"(${renderArg(p)}) failed to parse input ${literalize(v)} with $ex"
              case Router.Result.ParamError.DefaultFailed(p, ex) =>
                s"(${renderArg(p)})'s default value failed to evaluate with $ex"
            }.mkString("\n") + "\n" + s"expected arguments: $expectedMsg"
        ))
    }
  }

  def renderArg(arg: ArgSig) = backtickWrap(arg.name) + ": " + arg.typeString


  def entryDetails(ep: EntryPoint) = {
    ep.argSignatures.collect{
      case ArgSig(name, tpe, Some(doc), default) =>
        "\n" + name + " // " + doc
    }.mkString
  }

  /**
    * Additional [[scopt.Read]] instance to teach it how to read Ammonite paths
    */
  implicit def pathScoptRead: scopt.Read[Path] = scopt.Read.stringRead.map(Path(_, cwd))

}
