package ammonite.repl.interp
import java.io.File

import acyclic.file
import ammonite.ops.{read, _}
import ammonite.repl.Parsers.ImportTree
import ammonite.repl.tools.IvyThing
import ammonite.repl._

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
                      imports: Imports) extends Result
    case class ClassPath(file: Path) extends Result
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
  object File extends ImportHook {
    // import $file.foo.Bar, to import the file `foo/Bar.scala`
    def handle(source: ImportHook.Source, tree: ImportTree, interp: InterpreterInterface) = {
      val relative =
        tree.prefix
            .map{case "$up" => up; case x => ammonite.ops.empty/x}
            .reduce(_/_)


      source match{
        case Source.File(currentScriptPath) =>

          val relativeModules = tree.mappings match{
            case None => Seq(relative -> None)
            case Some(mappings) => for((k, v) <- mappings) yield relative/k -> v
          }
          def relToFile(x: RelPath) = currentScriptPath/up/x/up/s"${x.last}.scala"
          val files = relativeModules.map(x => relToFile(x._1))
          val missing = files.filter(!exists(_))

          if (missing.nonEmpty) {
            Res.Failure(None, "Cannot resolve $file import: " + missing.mkString(","))
          } else {
            Res.Success(
              for(((relativeModule, rename), filePath) <- relativeModules.zip(files)) yield {
                val (pkg, wrapper) = Util.pathToPackageWrapper(filePath)
                val fullPrefix = pkg.map(_.raw) ++ Seq(wrapper.raw)

                val importData = Seq(ImportData(
                  fullPrefix.last, rename.getOrElse(relativeModule.last),
                  fullPrefix.dropRight(1).map(Name), ImportData.TermType
                ))

                Result.Source(
                  read(filePath),
                  wrapper,
                  pkg,
                  ImportHook.Source.File(filePath),
                  Imports(importData)
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
        Imports(Seq(ImportData(url, target, Seq(Name("$url")), ImportData.Term)))
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
      val jars = resolved.flatten.map(Path(_)).map(Result.ClassPath)
      jars
    }
  }
}