package ammonite.interp.script

import java.io.File

import ammonite.compiler.iface.{CodeWrapper, Parser}
import ammonite.interp.{DependencyLoader, Interpreter}
import ammonite.runtime.{Frame, ImportHook, Storage}
import ammonite.util.{ImportTree, Name, PositionOffsetConversion, Util}
import ammonite.util.Util.CodeSource
import coursierapi.{Dependency, Repository}

import scala.collection.mutable

final case class ScriptProcessor(
    scalaVersion: String,
    parser: Parser,
    codeWrapper: CodeWrapper,
    dependencyLoader: DependencyLoader,
    defaultRepositories: Seq[Repository],
    extraPluginDependencies: Seq[Dependency] = Nil,
    wd: os.Path = os.pwd,
    importHooks: Map[Seq[String], ImportHook] = ImportHook.defaults
) { self =>

  import ScriptProcessor._

  def load(
      code: String,
      codeSource: CodeSource
  ): Script = {

    val rawCode = Interpreter.skipSheBangLine(code)
    lazy val offsetToPos = PositionOffsetConversion.offsetToPos(rawCode)
    val splittedScript = parser.scriptBlocksWithStartIndices(
      rawCode,
      codeSource.fileName
    ).left.map { f =>
      val startPos = offsetToPos(f.index).copy(char = 0)
      val endPos = {
        val endIdx = rawCode.indexOf('\n', f.index)
        val actualEndIdx = if (endIdx < 0) rawCode.length else endIdx
        offsetToPos(actualEndIdx)
      }
      Seq(Diagnostic("ERROR", startPos, endPos, s"Expected ${f.expected}"))
    }

    def hookFor(tree: ImportTree): Either[Diagnostic, (Seq[String], ImportHook)] = {
      val hookOpt = importHooks.find { case (k, v) => tree.strippedPrefix.startsWith(k) }
      hookOpt.toRight {
        Diagnostic(
          "ERROR",
          offsetToPos(tree.start),
          offsetToPos(tree.end),
          s"Invalid import hook '${tree.strippedPrefix.mkString(".")}'"
        )
      }
    }

    def hookResults(
        hookPrefix: Seq[String],
        hook: ImportHook,
        tree: ImportTree
    ): Either[Diagnostic, Seq[ImportHook.Result]] = {
      val r = hook.handle(
        codeSource,
        tree.copy(prefix = tree.prefix.drop(hookPrefix.length)),
        dummyInterpreterInterface,
        codeWrapper.wrapperPath
      )
      r.left.map { error =>
        Diagnostic(
          "ERROR",
          offsetToPos(tree.start),
          offsetToPos(tree.end),
          error
        )
      }
    }

    val diagnostics = new mutable.ListBuffer[Diagnostic]

    for (l <- splittedScript.left)
      diagnostics ++= l

    val blocks = for {
      elems <- splittedScript.right.toSeq
      Parser.ScriptBlock(startIdx, leadingSpaces, statements) <- elems
    } yield {
      val (statements0, importTrees) = parser.parseImportHooksWithIndices(codeSource, statements)
      val importResults =
        for {
          tree <- importTrees
          (hookPrefix, hook) <- hookFor(tree) match {
            case Left(diag) => diagnostics += diag; Nil
            case Right(imports0) => Seq(imports0)
          }
          res <- hookResults(hookPrefix, hook, tree) match {
            case Left(diag) => diagnostics += diag; Nil
            case Right(res) => res
          }
        } yield res
      Script.Block(startIdx, leadingSpaces, statements0, importResults)
    }

    Script(code, codeSource, blocks, diagnostics.toVector)
  }

  def load(path: os.Path, codeSource: CodeSource): Script = {
    val code = os.read(path)
    load(code, codeSource)
  }

  def load(path: os.Path): Script = {
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
          val deps = h.dependencies.scriptDependencies.map { imp =>
            imp.code match {
              case Left(code) => load(code, imp.codeSource)
              case Right(path) => load(path, imp.codeSource)
            }
          }

          val filteredDeps = deps.filterNot(alreadySeen)
          if (filteredDeps.isEmpty) {
            if (h != module)
              b += h
            helper(t, alreadySeen + h)
          } else
            helper(filteredDeps.toList ::: toAdd, alreadySeen)
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

  private val dummyInterpreterInterface =
    new ImportHook.InterpreterInterface {
      def loadIvy(coordinates: Dependency*): Either[String, Seq[File]] = Right(Nil)
      def watch(p: os.Path): Unit = ()
      def scalaVersion = self.scalaVersion
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
}
