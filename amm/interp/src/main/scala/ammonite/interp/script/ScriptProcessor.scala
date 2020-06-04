package ammonite.interp.script

import java.io.File

import ammonite.interp.{CodeWrapper, DependencyLoader, Interpreter, Parsers, Preprocessor}
import ammonite.runtime.{Frame, ImportHook, Storage}
import ammonite.util.{ImportTree, Name, Util}
import ammonite.util.Util.CodeSource
import coursierapi.{Dependency, Repository}

import scala.collection.mutable

final case class ScriptProcessor(
  dependencyLoader: DependencyLoader,
  defaultRepositories: Seq[Repository],
  extraPluginDependencies: Seq[Dependency] = Nil,
  wd: os.Path = os.pwd,
  codeWrapper: CodeWrapper = CodeWrapper,
  importHooks: Map[Seq[String], ImportHook] = ImportHook.defaults
) {

  import ScriptProcessor._

  def load(
    code: String,
    codeSource: CodeSource
  ): Either[String, Script] = {

    val splittedScript = Preprocessor.splitScriptWithStart(
      Interpreter.skipSheBangLine(code),
      codeSource.fileName
    )

    def hookFor(tree: ImportTree) = {
      val hookOpt = importHooks.find { case (k, v) => tree.strippedPrefix.startsWith(k) }
      hookOpt match {
        case None => Left(s"Invalid import hook '${tree.strippedPrefix.mkString(".")}'")
        case Some(hook) => Right((tree, hook))
      }
    }

    def hookResults(hookPrefix: Seq[String], hook: ImportHook, tree: ImportTree) =
      hook.handle(
        codeSource,
        tree.copy(prefix = tree.prefix.drop(hookPrefix.length)),
        ScriptProcessor.dummyInterpreterInterface,
        codeWrapper.wrapperPath
      )

    for {
      elems <- splittedScript
      withImportHooks <- elems
        .traverse {
          case (startIdx, leadingSpaces, statements) =>
            val (statements0, importTrees) = Parsers.parseImportHooks(codeSource, statements)
            importTrees.traverse(hookFor).map((startIdx, leadingSpaces, statements0, _))
        }
        .left.map(_.mkString(", "))
      r <-  withImportHooks
        .traverse {
          case (startIdx, leadingSpaces, statements0, imports) =>
            imports
              .traverse {
                case (tree, (hookPrefix, hook)) =>
                  hookResults(hookPrefix, hook, tree)
              }
              .map(l => Script.Block(startIdx, leadingSpaces, statements0, l.flatten))
        }
        .left.map(_.mkString(", "))
    } yield Script(code, codeSource, r)
  }

  def load(path: os.Path, codeSource: CodeSource): Either[String, Script] = {
    val code = os.read(path)
    load(code, codeSource)
  }

  def load(path: os.Path): Either[String, Script] = {
    val (pkg, wrapper) = Util.pathToPackageWrapper(Nil, path.relativeTo(wd))
    val codeSource = CodeSource(
      wrapper,
      pkg,
      Seq(Name("ammonite"), Name("$file")),
      Some(path)
    )
    val code = os.read(path)
    load(code, codeSource)
  }

  def dependencies(module: Script): Either[String, Seq[Script]] = {

    val b = new mutable.ListBuffer[Script]
    def helper(toAdd: List[Script], alreadySeen: Set[Script]): Either[String, List[Script]] =
      toAdd match {
        case Nil => Right(b.result())
        case h :: t if alreadySeen(h) =>
          helper(t, alreadySeen)
        case h :: t =>
          val maybeDeps = h.dependencies.scriptDependencies
            .traverse { imp =>
              imp.code match {
                case Left(code) => load(code, imp.codeSource)
                case Right(path) => load(path, imp.codeSource)
              }
            }

            maybeDeps match {
              case Left(errors) => Left(errors.mkString(", "))
              case Right(deps) =>
                val filteredDeps = deps.filterNot(alreadySeen)
                if (filteredDeps.isEmpty) {
                  if (h != module)
                    b += h
                  helper(t, alreadySeen + h)
                } else
                  helper(filteredDeps.toList ::: toAdd, alreadySeen)
            }
      }

    helper(module :: Nil, Set.empty)
  }

  def jarDependencies(module: Script): Either[String, Seq[os.Path]] =
    dependencies(module).flatMap { modules =>
      val deps = ScriptProcessor.mergeClasspathDependencies(module +: modules)

      dependencyLoader.load(deps.dependencies, defaultRepositories ++ deps.extraRepositories, Nil)
        .map(files => files.map(os.Path(_)) ++ deps.jarDependencies)
    }

  def jarPluginDependencies(module: Script): Either[String, Seq[os.Path]] =
    dependencies(module).flatMap { modules =>
      val deps = ScriptProcessor.mergeClasspathDependencies(module +: modules)

      dependencyLoader
        .load(
          deps.pluginDependencies ++ extraPluginDependencies,
          defaultRepositories ++ deps.extraRepositories,
          Nil
        )
        .map { files =>
          files.map(os.Path(_)) ++ deps.jarPluginDependencies
        }
    }

}

object ScriptProcessor {

  def mergeClasspathDependencies(modules: Seq[Script]): Script.Dependencies =
    modules
      .map(_.dependencies.copy(scriptDependencies = Nil))
      .foldLeft(Script.Dependencies())(_ + _)


  private[interp] implicit class SeqOps[T](private val l: Seq[T]) extends AnyVal {
    def traverse[L, R](f: T => Either[L, R]): Either[Seq[L], Seq[R]] = {
      val lefts = new mutable.ListBuffer[L]
      val rights = new mutable.ListBuffer[R]
      for (t <- l)
        f(t) match {
          case Left(l) =>
            lefts.append(l)
            if (rights.nonEmpty)
              rights.clear()
          case Right(r) =>
            if (lefts.isEmpty)
              rights.append(r)
        }
      if (lefts.isEmpty)
        Right(rights.toList)
      else
        Left(lefts.toList)
    }
  }

  private val dummyInterpreterInterface =
    new ImportHook.InterpreterInterface {
      def loadIvy(coordinates: Dependency*): Either[String, Seq[File]] = Right(Nil)
      def watch(p: os.Path): Unit = ()
      def addRepository(repository: Repository): Unit = ()
    }
}
