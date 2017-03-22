package ammonite.runtime

import java.io.File

import acyclic.file
import ammonite.ops.{read, _}
import ammonite.runtime.tools.IvyThing
import ammonite.util._
import org.jboss.shrinkwrap.resolver.api.maven.{Maven => MavenResolver}

/**
  * An extensible hook into the Ammonite REPL's import system; allows the end
  * user to hook into `import $foo.bar.{baz, qux => qua}` syntax, and in
  * response load jars or process source files before the "current" compilation
  * unit is run. Can be used to load script files, ivy dependencies, jars, or
  * files from the web.
  */
trait ImportHook{
  def handle(source: ImportHook.Source,
             tree: ImportTree,
             interp: ImportHook.InterpreterInterface): Res[Seq[ImportHook.Result]]
}

object ImportHook{

  /**
    * The minimal interface that is exposed to the import hooks from the
    * Interpreter. Open for extension, if someone needs more stuff, but by
    * default this is what is available.
    */
  trait InterpreterInterface{
    def wd: Path
    def loadIvy(coordinates: (String, String, String), verbose: Boolean = true): Set[File]
  }

  /**
    * The result of processing an [[ImportHook]]. Can be either a source-file
    * to evaluate, or additional files/folders/jars to put on the classpath
    */
  sealed trait Result
  object Result{
    case class Source(code: String,
                      wrapper: Name,
                      pkg: Seq[Name],
                      source: ImportHook.Source,
                      imports: Imports,
                      exec: Boolean) extends Result
    case class ClassPath(file: Path, plugin: Boolean) extends Result
  }

  /**
    * Where a script can "come from". Used to resolve relative $file imports
    * relative to the importing script.
    */
  sealed trait Source
  object Source{
    case class File(path: Path) extends Source
    case class URL(path: String) extends Source
  }
  object File extends SourceHook(false)
  object Exec extends SourceHook(true)

  def resolveFiles(tree: ImportTree, currentScriptPath: Path, extensions: Seq[String])
                  : (Seq[(RelPath, Option[String])], Seq[Path], Seq[Path]) = {
    val relative =
      tree.prefix
        .map{case ammonite.util.Util.upPathSegment => up; case x => ammonite.ops.empty/x}
        .reduce(_/_)
    val relativeModules = tree.mappings match{
      case None => Seq(relative -> None)
      case Some(mappings) => for((k, v) <- mappings) yield relative/k -> v
    }
    def relToFile(x: RelPath) = {
      val base = currentScriptPath/up/x/up/x.last
      extensions.find(ext => exists! base/up/(x.last + ext)) match{
        case Some(p) => Right(base/up/(x.last + p): Path)
        case None => Left(base)
      }

    }
    val resolved = relativeModules.map(x => relToFile(x._1))
    val missing = resolved.collect{case Left(p) => p}
    val files = resolved.collect{case Right(p) => p}
    (relativeModules, files, missing)
  }
  class SourceHook(exec: Boolean) extends ImportHook {
    // import $file.foo.Bar, to import the file `foo/Bar.sc`
    def handle(source: ImportHook.Source, tree: ImportTree, interp: InterpreterInterface) = {

      source match{
        case Source.File(currentScriptPath) =>

          val (relativeModules, files, missing) = resolveFiles(
            tree, currentScriptPath, Seq(".sc")
          )

          if (missing.nonEmpty) {
            Res.Failure(None, "Cannot resolve $file import: " + missing.mkString(", "))
          } else {
            Res.Success(
              for(((relativeModule, rename), filePath) <- relativeModules.zip(files)) yield {
                val (pkg, wrapper) = Util.pathToPackageWrapper(filePath, interp.wd)
                val fullPrefix = pkg ++ Seq(wrapper)

                val importData = Seq(ImportData(
                  fullPrefix.last, Name(rename.getOrElse(relativeModule.last)),
                  fullPrefix.dropRight(1), ImportData.TermType
                ))

                Result.Source(
                  Util.normalizeNewlines(read(filePath)),
                  wrapper,
                  pkg,
                  ImportHook.Source.File(filePath),
                  Imports(importData),
                  exec
                )
              }
            )
          }
        case Source.URL(path) => ???
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
        Imports(Seq(ImportData(Name(url), Name(target), Seq(Name("$url")), ImportData.Term))),
        false
      ))
    }
    // import $url.{ `http://www.google.com` => foo }
    def handle(source: ImportHook.Source, tree: ImportTree, interp: InterpreterInterface) = {
      tree.mappings match{
        case None => Res.Failure(None, "$url import failed for " + tree)
        case Some(mappings) =>
          Res.map(tree.mappings.get){ case (k, v) => resolveHttp(k, v.getOrElse(k)) }
      }
    }
  }
  object Ivy extends BaseIvy(plugin = false)
  object PluginIvy extends BaseIvy(plugin = true)
  class BaseIvy(plugin: Boolean) extends ImportHook{
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
      resolved.flatten.map(Path(_)).map(Result.ClassPath(_, plugin))
    }
  }


  object Maven extends ImportHook{
    def splitImportTree(tree: ImportTree): Res[Seq[String]] = {
      tree match{
        case ImportTree(Seq(part), None, _, _) => Res.Success(Seq(part))
        case ImportTree(Nil, Some(mapping), _, _) if mapping.map(_._2).forall(_.isEmpty) =>
          Res.Success(mapping.map(_._1))
        case _ => Res.Failure(None, "Invalid $maven import " + tree)
      }
    }
    def resolve(interp: InterpreterInterface, signature: String) = for{
      (a, b, c) <-  signature.split(':') match{
        case Array(a, b, c) => Res.Success((a, b, c))
        case Array(a, "", b, c) => Res.Success((a, b + "_" + IvyThing.scalaBinaryVersion, c))
        case _ => Res.Failure(None, s"Invalid $$maven import: [$signature]")
      }
      jars <- {
        try {
          val resolved = MavenResolver.resolver()
            .resolve(s"${a}:${b}:${c}")
            .withTransitivity().asFile()
          Res.Success(resolved)
        } catch {case ex =>
          Res.Exception(ex, "")
        }
      }
    } yield jars

    def handle(source: ImportHook.Source, tree: ImportTree, interp: InterpreterInterface) = for{
    // import $maven.`com.lihaoyi:scalatags_2.11:0.5.4`
      parts <- splitImportTree(tree)
      resolved <- Res.map(parts)(resolve(interp, _))
    } yield {
      resolved.flatten.map(Path(_)).map(Result.ClassPath(_, false))
    }
  }


  object Classpath extends BaseClasspath(plugin = false)
  object PluginClasspath extends BaseClasspath(plugin = true)
  class BaseClasspath(plugin: Boolean) extends ImportHook{
    def handle(source: ImportHook.Source, tree: ImportTree, interp: InterpreterInterface) = {
      source match{
        case Source.File(currentScriptPath) =>
          val (relativeModules, files, missing) = resolveFiles(
            tree, currentScriptPath, Seq(".jar", "")
          )

          if (missing.nonEmpty) {
            Res.Failure(None, "Cannot resolve $cp import: " + missing.mkString(", "))
          } else Res.Success(
            for(((relativeModule, rename), filePath) <- relativeModules.zip(files))
            yield Result.ClassPath(filePath, plugin)
          )
        case Source.URL(path) => ???
      }
    }
  }
}