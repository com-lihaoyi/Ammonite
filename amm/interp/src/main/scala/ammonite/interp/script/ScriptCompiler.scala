package ammonite.interp.script

import ammonite.interp.{
  CodeWrapper,
  Compiler => AmmCompiler,
  DefaultPreprocessor,
  Interpreter,
  MakeReporter
}
import ammonite.runtime.{Classpath, Frame, Storage}
import ammonite.util.{Imports, Name, Printer, Res}

import scala.collection.mutable
import scala.reflect.internal.util.{NoPosition, Position => SPosition}
import scala.reflect.io.VirtualDirectory
import scala.tools.nsc.Settings

final case class ScriptCompiler(
  storage: Storage,
  printer: Printer, // TODO Remove this
  codeWrapper: CodeWrapper,
  initialClassLoader: ClassLoader,
  initialImports: Imports,
  classPathWhitelist: Set[Seq[String]],
  wd: Option[os.Path],
  target: Option[os.Path],
  generateSemanticDbs: Boolean
) {

  import ScriptProcessor.SeqOps

  def compile(
    module: Script,
    processor: ScriptProcessor
  ): (
    Map[Script, Seq[Diagnostic]],
    Either[String, Seq[AmmCompiler.Output]]
  ) = {

    val diagnostics = new mutable.HashMap[Script, Seq[Diagnostic]]

    val res = for {
      dependencies <- processor.dependencies(module)
      depsOutput <- dependencies
        .filter(_ != module)
        .traverse { mod =>
          // recurses, beware
          // and not stack safe…
          val (diagnostics0, res0) = compile(mod, processor)
          diagnostics ++= diagnostics0
          res0
        }
        .left.map(_.mkString(", "))
        .map(_.flatten)

      jars <- processor.jarDependencies(module)
      pluginJars <- processor.jarPluginDependencies(module)

      resolvedDeps = Script.ResolvedDependencies(jars, pluginJars, depsOutput.flatMap(_.classFiles))

      output <- {
        val (moduleDiagnostics, moduleRes) = compile(module, resolvedDeps)
        diagnostics += module -> moduleDiagnostics
        moduleRes
      }
    } yield output

    (diagnostics.toMap, res)
  }

  private def segments(module: Script): Option[Seq[String]] =
    for {
      p <- module.codeSource.path
      segments = wd.fold(p.segments.toVector)(wd0 => p.relativeTo(wd0).segments.toVector)
    } yield segments

  def moduleOutput(module: Script): Option[os.Path] =
    for {
      target0 <- target
      segments0 <- segments(module)
      dest = target0 / segments0.init / segments0.last.stripSuffix(".sc")
    } yield dest

  def moduleSources(module: Script): Option[os.Path] =
    moduleOutput(module).map(_ / "src")
  def moduleTarget(module: Script): Option[os.Path] =
    moduleOutput(module).map(_ / "target")

  private def writeSource(module: Script, clsName: Name, code: String): Option[Seq[String]] =
    for (dir <- moduleSources(module)) yield {
      // Using Name.raw rather than Name.encoded, as all those names
      // (except the ammonite.$file prefix) originate from file paths,
      // and are thus safe to use as is in paths.
      // BSP clients can also find those files themselves, without the encoding logic.
      val relPath = module.codeSource.pkgName.map(_.raw) :+ s"${clsName.raw}.scala"
      val dest = dir / relPath
      os.write.over(dest, code, createFolders = true)
      relPath
    }

  private def clearByteCodeDir(module: Script): Unit =
    for (dest <- moduleTarget(module))
      try os.remove.all(dest)
      catch {
        case _: java.nio.file.DirectoryNotEmptyException =>
          // Can happen on Windows, if any of the file we try to delete is opened by the BSP
          // client.
      }

  private def writeByteCode(module: Script, byteCode: Seq[(String, Array[Byte])]): Unit =
    for (dest <- moduleTarget(module)) {
      os.makeDir.all(dest)
      for ((name, b) <- byteCode) {
        val parts = name.split('.').toSeq
        val dest0 = dest / parts.init / s"${parts.last}.class"
        os.write.over(dest0, b, createFolders = true)
      }
    }

  private def updateSemanticDbs(
    module: Script,
    wd: Option[os.Path],
    adjust: Int => (Int, Int) => Option[(Int, Int)]
  ): Unit =
    for {
      target <- moduleTarget(module)
      segments0 <- segments(module)
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

  def moduleSettings(module: Script): List[String] =
    if (generateSemanticDbs)
      List(
        "-Yrangepos",
        "-P:semanticdb:failures:warning",
        "-P:semanticdb:synthetics:on"
      ) ++
        moduleSources(module).map(d => s"-P:semanticdb:sourceroot:${d.toNIO.toAbsolutePath}") ++
        moduleTarget(module).map(d => s"-P:semanticdb:targetroot:${d.toNIO.toAbsolutePath}")
    else
      Nil

  private def settingsOrError(module: Script): Either[Seq[String], Settings] = {

    val args = moduleSettings(module)

    val errors = new mutable.ListBuffer[String]
    val settings0 = new Settings(err => errors += err)
    val (_, unparsed) = settings0.processArguments(args, true)
    for (arg <- unparsed)
      errors += s"Unrecognized argument: $arg"

    if (errors.isEmpty)
      Right(settings0)
    else
      Left(errors.toList)
  }

  def preCompile(module: Script): Unit = {

    val settings0 = settingsOrError(module).getOrElse(new Settings(_ => ()))

    val reporter = MakeReporter.makeReporter(
      (_, _) => (),
      (_, _) => (),
      (_, _) => (),
      settings0
    )

    val initialClassPath = Classpath.classpath(initialClassLoader, storage)
    val compiler = AmmCompiler(
      initialClassPath,
      new VirtualDirectory("(memory)", None),
      initialClassLoader,
      initialClassLoader,
      () => (),
      Some(reporter),
      settings0,
      Set.empty,
      initialClassPath,
      lineNumberModifier = false
    )

    val dependencyImports = initialImports ++ module.dependencyImports

    val preprocessor = new DefaultPreprocessor(
      compiler.parse(module.codeSource.fileName, _),
      markGeneratedSections = true
    )

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
        _ = writeSource(module, indexedWrapperName, processed.code)
      } yield ()
    }
  }

  private def doCompile(
    settings0: Settings,
    module: Script,
    dependencies: Script.ResolvedDependencies
  ): (
    Seq[Diagnostic],
    Either[String, Seq[AmmCompiler.Output]]
  ) = {

    // TODO Cache compilation results

    val dynamicClasspath = new VirtualDirectory("(memory)", None)
    val frame = Frame.createInitial(initialClassLoader)

    frame.addClasspath(dependencies.jars.map(_.toNIO.toUri.toURL))
    frame.addPluginClasspath(dependencies.pluginJars.map(_.toNIO.toUri.toURL))
    for ((clsName, byteCode) <- dependencies.byteCode)
      frame.classloader.addClassFile(clsName, byteCode)

    AmmCompiler.addToClasspath(dependencies.byteCode, dynamicClasspath)

    var messages = new mutable.ListBuffer[Diagnostic]
    var newMessages = new mutable.ListBuffer[(String, Int, Int, String)]
    def flushMessages(indexToPos: Int => Position): Unit = {
      newMessages.foreach {
        case (severity, start, end, msg) =>
          val startPos = indexToPos(start)
          val endPos = indexToPos(end)
          messages.append(Diagnostic(severity, startPos, endPos, msg))
      }
      newMessages.clear()
    }

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
        settings0
      )
    }

    val initialClassPath = Classpath.classpath(initialClassLoader, storage)
    val classPath = Classpath.classpath(frame.classloader, storage)
    val compiler = AmmCompiler(
      classPath,
      dynamicClasspath,
      frame.classloader,
      frame.pluginClassloader,
      () => (),
      Some(reporter),
      settings0,
      classPathWhitelist,
      initialClassPath,
      lineNumberModifier = false
    )

    val dependencyImports = initialImports ++ module.dependencyImports

    val preprocessor = new DefaultPreprocessor(
      compiler.parse(module.codeSource.fileName, _),
      markGeneratedSections = true
    )

    // not sure the indices will be correct if there are several blocks…

    val offsetToPosSc = PositionOffsetConversion.offsetToPos(module.code)

    clearByteCodeDir(module)
    val start = (Imports(), List.empty[(Int, String, AmmCompiler.Output)])
    val res = Res.fold(start, module.blocks.zipWithIndex) {
      case ((scriptImports, acc), (block, blockIdx)) =>
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

            val relPathOpt = writeSource(module, indexedWrapperName, processed.code)

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
        } yield (scriptImports ++ output.imports, (offset, processedCode, output) :: acc)
    }

    val finalRes = res.map(_._2.toVector) match {
      case Res.Failure(msg) => Left(msg)
      case Res.Skip =>
        writeByteCode(module, Nil)
        Right(Nil)
      case Res.Success(output) =>
        writeByteCode(module, output.flatMap(_._3.classFiles))
        if (generateSemanticDbs)
          updateSemanticDbs(
            module,
            wd,
            blockIdx => {
              val startOffsetInSc = module.blocks(blockIdx - 1).startIdx
              val startPosInSc = offsetToPosSc(startOffsetInSc)
              assert(startPosInSc.char == 0, s"wrong startPosInSc $startPosInSc")

              PositionOffsetConversion.scalaPosToScPos(
                module.code,
                startPosInSc.line,
                output(blockIdx - 1)._2,
                output(blockIdx - 1)._1
              )
            }
          )
        Right(output.map(_._3))
      case Res.Exception(ex, msg) => ???
      case Res.Exit(_) => ???
    }

    (messages.toList, finalRes)
  }

  def compile(
    module: Script,
    dependencies: Script.ResolvedDependencies
  ): (
    Seq[Diagnostic],
    Either[String, Seq[AmmCompiler.Output]]
  ) =
    settingsOrError(module) match {
      case Left(errors) => (Nil, Left(errors.mkString(", ")))
      case Right(settings) => doCompile(settings, module, dependencies)
    }
}
