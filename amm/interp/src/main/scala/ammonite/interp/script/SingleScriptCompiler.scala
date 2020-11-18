package ammonite.interp.script

import ammonite.interp.{
  CodeWrapper,
  Compiler => AmmCompiler,
  Interpreter,
  MakeReporter
}
import ammonite.runtime.{Classpath, Frame, Storage}
import ammonite.util.{Imports, Name, Printer, Res}

import scala.collection.mutable
import scala.reflect.internal.util.{NoPosition, Position => SPosition}
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.Settings

/**
 * Helper class to compile a single script
 *
 * Only meant to be used to compile a script once. Should be
 * discarded right after having called `apply` or `writeSources`.
 */
class SingleScriptCompiler(
  initialClassLoader: ClassLoader,
  storage: Storage,
  printer: Printer,
  initialImports: Imports,
  classPathWhitelist: Set[Seq[String]],
  codeWrapper: CodeWrapper,
  wd: Option[os.Path],
  generateSemanticDbs: Boolean,
  settings: Settings,
  module: Script,
  dependencies: Script.ResolvedDependencies,
  moduleTarget: Option[os.Path],
  moduleSources: Option[os.Path]
) {

  private val dynamicClasspath = {
    val vd = new VirtualDirectory("(memory)", None)
    AmmCompiler.addToClasspath(dependencies.byteCode, vd)
    vd
  }

  private val frame = {
    val f = Frame.createInitial(initialClassLoader)
    f.addClasspath(dependencies.jars.map(_.toNIO.toUri.toURL))
    f.addPluginClasspath(dependencies.pluginJars.map(_.toNIO.toUri.toURL))
    for ((clsName, byteCode) <- dependencies.byteCode)
      f.classloader.addClassFile(clsName, byteCode)
    f
  }

  private var messages = new mutable.ListBuffer[Diagnostic]
  private var newMessages = new mutable.ListBuffer[(String, Int, Int, String)]
  private def flushMessages(indexToPos: Int => Position): Unit = {
    newMessages.foreach {
      case (severity, start, end, msg) =>
        val startPos = indexToPos(start)
        val endPos = indexToPos(end)
        messages.append(Diagnostic(severity, startPos, endPos, msg))
    }
    newMessages.clear()
  }

  private val compiler = {

    val reporter = {
      def add(pos: SPosition, msg: String, severity: String) =
        if (pos == NoPosition)
          newMessages.append((severity, 0, 0, msg))
        else
          newMessages.append((severity, pos.start, pos.end, msg))
      MakeReporter.makeReporter(
        (pos, msg) => add(pos, msg, "ERROR"),
        (pos, msg) => add(pos, msg, "WARNING"),
        (pos, msg) => add(pos, msg, "INFO"),
        settings
      )
    }

    val initialClassPath = Classpath.classpath(
      initialClassLoader,
      storage.dirOpt.map(_.toNIO)
    )
    val classPath = Classpath.classpath(
      frame.classloader,
      storage.dirOpt.map(_.toNIO)
    )

    AmmCompiler(
      classPath,
      dynamicClasspath,
      frame.classloader,
      frame.pluginClassloader,
      () => (),
      Some(reporter),
      settings,
      classPathWhitelist,
      initialClassPath,
      lineNumberModifier = false
    )
  }

  private val dependencyImports = initialImports ++ module.dependencyImports

  private val preprocessor = compiler.preprocessor(
    module.codeSource.fileName,
    markGeneratedSections = true
  )

  private val offsetToPosSc = PositionOffsetConversion.offsetToPos(module.code)


  private def clearByteCodeDir(): Unit =
    // remove only files from the target directory, not directories
    // (removing directories can confuse BSP clients with file watchers)
    for {
      dest <- moduleTarget
      if os.isDir(dest)
      file <- os.walk(dest, skip = os.isDir(_))
    } {
      os.remove(file)
    }

  private def writeSource(clsName: Name, code: String): Option[Seq[String]] =
    for (dir <- moduleSources) yield {
      // Using Name.raw rather than Name.encoded, as all those names
      // (except the ammonite.$file prefix) originate from file paths,
      // and are thus safe to use as is in paths.
      // BSP clients can also find those files themselves, without the encoding logic.
      val relPath = module.codeSource.pkgName.map(_.raw) :+ s"${clsName.raw}.scala"
      val dest = dir / relPath
      os.write.over(dest, code, createFolders = true)
      relPath
    }

  private def writeByteCode(byteCode: Seq[(String, Array[Byte])]): Unit =
    for (dest <- moduleTarget) {
      os.makeDir.all(dest)
      for ((name, b) <- byteCode) {
        val parts = name.split('.').toSeq
        val dest0 = dest / parts.init / s"${parts.last}.class"
        os.write.over(dest0, b, createFolders = true)
      }
    }

  private def updateSemanticDbs(
    blocksOffsetAndCode: Vector[(Int, String)]
  ): Unit = {

    def adjust(blockIdx: Int): (Int, Int) => Option[(Int, Int)] =
      if (module.blocks.isEmpty) // can happen if there were errors during preprocessing
        (_, _) => None
      else {
        val startOffsetInSc = module.blocks(blockIdx - 1).startIdx
        val startPosInSc = offsetToPosSc(startOffsetInSc)

        PositionOffsetConversion.scalaPosToScPos(
          module.code,
          startPosInSc.line,
          startPosInSc.char,
          blocksOffsetAndCode(blockIdx - 1)._2,
          blocksOffsetAndCode(blockIdx - 1)._1
        )
      }

    for {
      target <- moduleTarget
      segments0 <- module.segments(wd)
    } {
      // TODO Merge the semantic DBs of all the blocks rather than just pick the last one
      val name = Interpreter.indexWrapperName(
        module.codeSource.wrapperName,
        module.blocks.length
      )
      // See comment above above in writeSource about the use of Name.raw rather than Name.encoded.
      val origRelPath = os.SubPath(
        module
          .codeSource
          .pkgName
          .map(_.raw)
          .toVector :+
        s"${name.raw}.scala"
      )
      val destRelPath = os.SubPath(segments0.toVector)

      SemanticdbProcessor.postProcess(module, wd, adjust, target, origRelPath, destRelPath)
    }
  }


  private def compileBlock(
    scriptImports: Imports,
    block: Script.Block,
    blockIdx: Int
  ): Res[(Imports, Int, String, AmmCompiler.Output)] = {

    val indexedWrapperName = Interpreter.indexWrapperName(
      module.codeSource.wrapperName,
      blockIdx + 1
    )

    for {
      // TODO Get diagnostics from preprocessing
      processed <- preprocessor.transform(
        block.statements,
        "",
        block.leadingSpaces,
        module.codeSource,
        indexedWrapperName,
        dependencyImports ++ scriptImports,
        _ => "scala.Iterator[String]()",
        extraCode = "",
        skipEmpty = false,
        markScript = true,
        codeWrapper = codeWrapper
      )

      outputOpt = {

        val relPathOpt = writeSource(indexedWrapperName, processed.code)

        val offsetInScala = processed.prefixCharLength
        val fileName = {
          val nameOpt = relPathOpt.map(_.mkString("/"))
          nameOpt.getOrElse(module.codeSource.fileName)
        }
        // TODO Make parsing errors start and end on the whole block?
        flushMessages { idxInScala =>
          val idxInSc = idxInScala - offsetInScala
          offsetToPosSc(block.startIdx + idxInSc)
        }
        val generatedCodeIndicesInScala =
          PositionOffsetConversion.sections(
            processed.code,
            "/*<amm>*/",
            "/*</amm>*/"
          ).toVector
        try {
          compiler.compile(
            processed.code.getBytes(scala.util.Properties.sourceEncoding), // encoding?
            printer,
            offsetInScala,
            processed.userCodeNestingLevel,
            fileName
          ).map((offsetInScala, processed.code, _))
        } finally {
          flushMessages { idxInScala =>
            val extraOffsetInScala =
              PositionOffsetConversion.extraOffset(
                generatedCodeIndicesInScala,
                idxInScala
              )
            val idxInSc = idxInScala - offsetInScala - extraOffsetInScala
            offsetToPosSc(block.startIdx + idxInSc)
          }
        }
      }

      (offset, processedCode, output) <- Res(outputOpt, "Compilation failed")
    } yield (scriptImports ++ output.imports, offset, processedCode, output) // :: acc)
  }

  private def compileBlocks(): Res[Seq[(Int, String, AmmCompiler.Output)]] = {
    val start = (Imports(), List.empty[(Int, String, AmmCompiler.Output)])
    val res = Res.fold(start, module.blocks.zipWithIndex) {
      case ((scriptImports, acc), (block, blockIdx)) =>
        compileBlock(scriptImports, block, blockIdx).map {
          case (newScriptImports, offset, processedCode, output) =>
            (newScriptImports, (offset, processedCode, output) :: acc)
        }
    }
    res.map(_._2)
  }

  def apply() = {

    clearByteCodeDir()

    val finalRes = compileBlocks() match {

      case Res.Failure(msg) =>
        Left(msg)

      case Res.Skip =>
        writeByteCode(Nil)
        Right(Nil)

      case Res.Success(output) =>
        writeByteCode(output.flatMap(_._3.classFiles))
        if (generateSemanticDbs) {
          val blocksOffsetAndCode = output
            .map { case (offset, code, _) => (offset, code) }
            .toVector
          updateSemanticDbs(blocksOffsetAndCode)
        }
        Right(output.map(_._3))

      case Res.Exception(ex, msg) =>
        // FIXME Shouldn't happen, compileBlocks isn't supposed to return a Res of that type
        Left(s"Unexpected exception while compiling block ($ex): $msg")
      case Res.Exit(_) =>
        // FIXME Shouldn't happen, compileBlocks isn't supposed to return a Res of that type
        Left("Unexpected exit call while compiling block")
    }

    ScriptCompileResult(messages.toList, finalRes)
  }

  def writeSources(): Unit =
    for ((block, blockIdx) <- module.blocks.zipWithIndex) {
      val indexedWrapperName = Interpreter.indexWrapperName(
        module.codeSource.wrapperName,
        blockIdx + 1
      )

      val res = preprocessor.transform(
        block.statements,
        "",
        block.leadingSpaces,
        module.codeSource,
        indexedWrapperName,
        dependencyImports,
        _ => "scala.Iterator[String]()",
        extraCode = "",
        skipEmpty = false,
        markScript = true,
        codeWrapper = codeWrapper
      )

      for {
        processed <- res
        _ = writeSource(indexedWrapperName, processed.code)
      } yield ()
    }

}
