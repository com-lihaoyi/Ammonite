package ammonite.repl.interp
import java.io.File

import acyclic.file
import ammonite.ops.{Path, exists, read, up}
import ammonite.repl.Parsers.ImportTree
import ammonite.repl.tools.IvyThing
import ammonite.repl.{Res, Util, Name}


trait ImportHook{
  def handle(tree: ImportTree, interp: ImportHook.InterpreterInterface): Res[Set[ImportHook.Result]]
}

object ImportHook{
  trait InterpreterInterface{
    def wd: Path
    def loadIvy(coordinates: (String, String, String), verbose: Boolean = true): Set[File]
  }
  sealed trait Result
  object Result{
    case class Source(code: String, wrapper: Name, pkg: Seq[Name]) extends Result
    case class Jar(file: Path) extends Result
  }
  object File extends ImportHook {
    // import $file.foo.Bar, to import the file `foo/Bar.scala`
    def handle(tree: ImportTree, interp: InterpreterInterface) = {
      def find(targetScript: Path, importSegments: Seq[String]): Option[Path] = {
        val possibleScriptPath = targetScript / up / s"${targetScript.last}.scala"
        if (exists ! possibleScriptPath) Some(possibleScriptPath)
        else importSegments match {
          case Seq() => None
          case Seq(first, rest@_*) => find(targetScript / first, rest)
        }
      }

      find(interp.wd, tree.prefix) match {
        case None => Res.Failure(None, "Cannot resolve import " + tree.prefix.mkString("."))
        case Some(scriptPath) =>
          val (pkg, wrapper) = Util.pathToPackageWrapper(scriptPath, interp.wd)
          Res.Success(Set(Result.Source(read(scriptPath), wrapper, pkg)))
      }
    }
  }

  object Http extends ImportHook{
    // import $url.{ `http://www.google.com` => foo }
    def handle(tree: ImportTree, interp: InterpreterInterface) = {
      tree.mappings match{
        case None => Res.Failure(None, "$url import failed for " + tree)
        case Some(mappings) =>
          Res.map(tree.mappings.get.toSet){ case (k, v) =>
            val res = scalaj.http.Http(k).asString
            if (!res.is2xx) Res.Failure(None, "$url import failed for " + k)
            else {
              val resx = Result.Source(res.body, Name(k), Seq(Name("$url")))
              Res.Success(resx)
            }
          }
      }
    }
  }

  object Ivy extends ImportHook{
    def splitImportTree(tree: ImportTree): Res[Seq[String]] = {
      tree match{
        case ImportTree(Seq(part), None) => Res.Success(Seq(part))
        case ImportTree(Nil, Some(mapping)) if mapping.map(_._2).forall(_.isEmpty) =>
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
    def handle(tree: ImportTree, interp: InterpreterInterface) = for{
    // import $ivy.`com.lihaoyi:scalatags_2.11:0.5.4`
      parts <- splitImportTree(tree)
      resolved <- Res.map(parts)(resolve(interp, _))
    } yield {
      // Code-gen a stub file so the original import has something it can
      // pretend to import
      val stub = Result.Source(
        "def x = ()",
        Name(tree.prefix.head),
        Seq(Name("$ivy"))
      )
      val jars: Set[Result.Jar] = resolved.flatten.map(Path(_)).map(Result.Jar).toSet
      jars ++ Set(stub)
    }
  }
}