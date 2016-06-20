package ammonite.repl.interp
import java.io.File

import acyclic.file
import ammonite.ops.{Path, exists, read, up}
import ammonite.repl.Parsers.ImportTree
import ammonite.repl.tools.IvyThing
import ammonite.repl._

import scala.annotation.tailrec


trait ImportHook{
  def handle(source: ImportHook.Source,
             tree: ImportTree,
             interp: ImportHook.InterpreterInterface): Res[Set[ImportHook.Result]]
}

object ImportHook{
  trait InterpreterInterface{
    def wd: Path
    def loadIvy(coordinates: (String, String, String), verbose: Boolean = true): Set[File]
  }
  sealed trait Result
  object Result{
    case class Source(code: String,
                      wrapper: Name,
                      pkg: Seq[Name],
                      source: ImportHook.Source,
                      imports: Imports) extends Result
    case class ClassPath(file: Path) extends Result
  }
  sealed trait Source
  object Source{
    case class File(path: Path) extends Source
    case class URL(path: String) extends Source
  }
  object File extends ImportHook {
    // import $file.foo.Bar, to import the file `foo/Bar.scala`
    def handle(source: ImportHook.Source, tree: ImportTree, interp: InterpreterInterface) = {
      val relative =
        tree.prefix
            .map{case "$up" => up; case x => ammonite.ops.empty/x}
            .reduce(_/_)

      source match{
        case Source.File(path) =>
          @tailrec def find(targetScript: Path, importSegments: Seq[String]): Option[Path] = {
            val possibleScriptPath = targetScript/up/s"${targetScript.last}.scala"
            if (exists ! possibleScriptPath) Some(possibleScriptPath)
            else importSegments match {
              case Seq() => None
              case Seq(first, rest@_*) => find(targetScript / first, rest)
            }
          }

          find(path/up/relative.copy(segments = Vector()), relative.segments) match {
            case None => Res.Failure(None, "Cannot resolve import " + tree.prefix.mkString("."))
            case Some(prefixPath) =>
              tree.mappings match{
                case None =>
                  val (pkg, wrapper) = Util.pathToPackageWrapper(prefixPath, interp.wd)
                  val importData = ImportData(wrapper.raw, wrapper.raw, pkg, ImportData.TermType)

                  Res.Success(Set(Result.Source(
                    read(prefixPath),
                    wrapper,
                    pkg,
                    ImportHook.Source.File(prefixPath),
                    Imports(Seq(importData))
                  )))
              }

          }
        case Source.URL(path) =>
          val Array(protocol, _, host, segments @ _*) = path.split("/", -1)
          val suffix = segments.last.split('.') match{
            case Array() => ""
            case parts => "." + parts.last
          }
          val relativized = ammonite.ops.empty/segments/relative/up
          if (relativized.ups > 0) Res.Failure(None, "")
          else Http.resolveHttp(
            protocol + "//" + host + "/" + segments.mkString("/") + suffix,
            ???
          ).map(Set(_))
      }
    }
  }


  object Http extends ImportHook{
    def resolveHttp(url: String, target: String) = {
      val res = scalaj.http.Http(url).asString
      if (!res.is2xx) Res.Failure(None, "$url import failed for " + url)
      else Res.Success(Result.Source(
        res.body,
        Name(url),
        Seq(Name("$url")),
        ImportHook.Source.URL(url),
        Imports(Seq(ImportData(url, target, Seq(Name("$url")), ImportData.Term)))
      ))
    }
    // import $url.{ `http://www.google.com` => foo }
    def handle(source: ImportHook.Source, tree: ImportTree, interp: InterpreterInterface) = {
      tree.mappings match{
        case None => Res.Failure(None, "$url import failed for " + tree)
        case Some(mappings) =>
          Res.map(tree.mappings.get.toSet){ case (k, v) => resolveHttp(k, v.getOrElse(k)) }
      }
    }
  }

  object Ivy extends ImportHook{
    def splitImportTree(tree: ImportTree): Res[Seq[String]] = {
      tree match{
        case ImportTree(Seq(part), None, _, _) => Res.Success(Seq(part))
        case ImportTree(Nil, Some(mapping), _, _) if mapping.map(_._2).forall(_.isEmpty) =>
          Res.Success(mapping.map(_._1))
        case _ => Res.Failure(None, "Invalid $ivy import " + tree)
      }
    }
    def resolve(interp: InterpreterInterface, signature: String) = for{
      (a, b, c) <-  signature.split(':') match{
        case Array(a, b, c) => Res.Success((a, b, c))
        case Array(a, "", b, c) => Res.Success((a, b + "_" + IvyThing.scalaBinaryVersion, c))
        case _ => Res.Failure(None, s"Invalid $$ivy import: [$signature]")
      }
      jars <- {
        try Res.Success(interp.loadIvy((a, b, c))) catch {case ex =>
          Res.Exception(ex, "")
        }
      }
    } yield jars

    def handle(source: ImportHook.Source, tree: ImportTree, interp: InterpreterInterface) = for{
    // import $ivy.`com.lihaoyi:scalatags_2.11:0.5.4`
      parts <- splitImportTree(tree)
      resolved <- Res.map(parts)(resolve(interp, _))
    } yield {
      // Code-gen a stub file so the original import has something it can
      // pretend to import
      val stub = Result.Source(
        "def x = ()",
        Name(tree.prefix.head),
        Seq(Name("$ivy")),
        ImportHook.Source.File(interp.wd),
        Imports()
      )
      val jars: Set[Result.ClassPath] = resolved.flatten.map(Path(_)).map(Result.ClassPath).toSet
      jars ++ Set(stub)
    }
  }
}