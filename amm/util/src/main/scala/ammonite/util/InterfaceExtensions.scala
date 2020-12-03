package ammonite.util

import ammonite.compiler.iface._
import ammonite.util.Name
import ammonite.util.Util.entry

import scala.collection.mutable

object InterfaceExtensions {

  implicit class CodeSourceExtensions(private val cs: CodeSource) extends AnyVal {

    def wrapperName = Name(cs.wrapper())
    def flexiblePkgName = cs.flexiblePkg.toSeq.map(Name(_))
    def pkgRoot = cs.rawPkgRoot.toSeq.map(Name(_))

    def pkgName = pkgRoot ++ flexiblePkgName
    def fullName = pkgName :+ wrapperName

    def fileName = path.fold(filePathPrefix.last + ".sc")(_.getFileName.toString)
    def jvmPathPrefix = fullName.map(_.encoded).mkString(".")
    def filePathPrefix = fullName.map(_.encoded)
    def printablePath = path match{
      case Some(x) => x.toString
      case None => "(synthetic)/" + filePathPrefix.mkString("/") + ".sc"
    }

    def path = Option(cs.pathOrNull)

    def withWrapperName(newWrapperName: Name): CodeSource =
      new CodeSource(newWrapperName.raw, cs.flexiblePkg, cs.rawPkgRoot, cs.pathOrNull)
  }

  implicit class CodeWrapperExtensions(private val wrapper: CodeWrapper) extends AnyVal {
    def wrapperPath = wrapper.rawWrapperPath.map(Name(_))
    def apply(
      code: String,
      source: CodeSource,
      imports: ammonite.compiler.iface.Imports,
      printCode: String,
      indexedWrapperName: Name,
      extraCode: String
    ): (String, String, Int) = {
      val e = wrapper.wrap(code, source, imports, printCode, indexedWrapperName.raw, extraCode)
      (e.getKey, e.getValue.getKey, e.getValue.getValue)
    }
  }

  implicit class CompilerExtensions(private val compiler: Compiler) extends AnyVal {
    def compile(src: Array[Byte],
                printer: Logger,
                importsLen0: Int,
                userCodeNestingLevel: Int,
                fileName: String): Option[Compiler.Output] =
      Option(compiler.compileOrNull(src, printer, importsLen0, userCodeNestingLevel, fileName))
  }

  implicit class CompilerOutputExtensions(private val output: Compiler.Output) extends AnyVal {
    def usedEarlierDefinitions = Option(output.usedEarlierDefinitionsOrNull).map(_.toSeq)
  }

  implicit class CompilerLifecycleManagerExtensions(
    private val self: CompilerLifecycleManager
  ) extends AnyVal {
    def complete(
      offset: Int,
      previousImports: String,
      snippet: String
    ): (Int, Seq[String], Seq[String]) = {
      val e = self.completions(offset, previousImports, snippet)
      (e.getKey, e.getValue.getKey, e.getValue.getValue)
    }
    def compileClass(
      processed: Preprocessor.Output,
      logger: Logger,
      fileName: String
    ): Option[Compiler.Output] =
      Option(self.compileClassOrNull(processed, logger, fileName))
  }

  implicit class PreprocessorExtensions(
    private val self: Preprocessor
  ) extends AnyVal {
    def transform(
      stmts: Seq[String],
      resultIndex: String,
      leadingSpaces: String,
      codeSource: CodeSource,
      indexedWrapper: Name,
      imports: Imports,
      printerTemplate: String => String,
      extraCode: String,
      skipEmpty: Boolean,
      markScript: Boolean,
      codeWrapper: CodeWrapper
    ): Res[Preprocessor.Output] =
      try {
        self.transformOrNull(
          stmts.toArray,
          resultIndex,
          leadingSpaces,
          codeSource,
          indexedWrapper.raw,
          imports,
          prints => printerTemplate(prints),
          extraCode,
          skipEmpty,
          markScript,
          codeWrapper
        ) match {
          case null => Res.Skip
          case res => Res.Success(res)
        }
      } catch {
        case e: Preprocessor.PreprocessorError =>
          Res.Failure(e.getMessage)
      }
  }

  implicit class ImportsExtensions(
    private val self: ammonite.compiler.iface.Imports
  ) extends AnyVal {
    def ++(others: ammonite.compiler.iface.Imports) =
      ammonite.util.Imports(self.data.toSeq, others.data.toSeq)

    def repr: String =
      ammonite.util.Imports.toString(self)
  }

  implicit class ImportsDataExtensions(
    private val data: ammonite.compiler.iface.Imports.Data
  ) extends AnyVal {
    def fromName: Name = Name(data.from)
    def toName: Name = Name(data.to)
    def prefix: Seq[Name] = data.rawPrefix.map(Name(_))
    def importType: ImportData.ImportType =
      data.`type` match {
        case ammonite.compiler.iface.Imports.Term => ImportData.Term
        case ammonite.compiler.iface.Imports.Type => ImportData.Type
        case ammonite.compiler.iface.Imports.TermType => ImportData.TermType
      }

    def withPrefix(newPrefix: Seq[Name]): ammonite.compiler.iface.Imports.Data =
      new ammonite.compiler.iface.Imports.Data(
        data.from,
        data.to,
        newPrefix.map(_.raw).toArray,
        data.`type`()
      )
  }

  implicit class ImportTreeExtensions(private val importTree: ImportTree) extends AnyVal {
    def mappings: Option[ImportMapping] =
      Option(importTree.mappingsOrNull).map { mappings =>
        mappings.toSeq.map { e =>
          (e.getKey, Option(e.getValue))
        }
      }
    def tupled: (Seq[String], Option[ImportMapping], Int, Int) =
      (importTree.prefix.toSeq, mappings, importTree.start, importTree.end)
  }

  type ImportMapping = Seq[(String, Option[String])]
  def importTree(
    prefix: Seq[String],
    mappings: Option[ImportMapping],
    start: Int,
    end: Int
  ): ImportTree =
    new ImportTree(
      prefix.toArray,
      mappings
        .map { seq =>
          seq
            .toArray
            .map { case (a, b) => entry(a, b.orNull) }
        }
        .orNull,
      start,
      end
    )

  implicit class ParserExtensions(private val parser: Parser) extends AnyVal {
    def parseImportHooks(source: CodeSource, stmts: Seq[String]): (Seq[String], Seq[ImportTree]) = {
      val statementsWithIndices = stmts.map { s =>
        entry(0: Integer, s)
      }
      val res = parser.importHooks(source, statementsWithIndices.toArray)
      (res.hookStatements.toSeq, res.importTrees.toSeq)
    }
    def parseImportHooksWithIndices(
      source: CodeSource,
      stmts: Seq[(Int, String)]
    ): (Seq[String], Seq[ImportTree]) = {
      val statementsWithIndices = stmts.map {
        case (idx, s) =>
          entry(idx: Integer, s)
      }
      val res = parser.importHooks(source, statementsWithIndices.toArray)
      (res.hookStatements.toSeq, res.importTrees.toSeq)
    }

    def splitScript(
      rawCode: String,
      fileName: String
    ): Either[String, IndexedSeq[(String, Seq[String])]] = {
      val res =
        try Right(parser.scriptBlocks(rawCode, fileName))
        catch {
          case e: Parser.ScriptSplittingError =>
            Left(e.getMessage)
        }
      res.map { blocks =>
        blocks.map { block =>
          (block.ncomment, block.codeWithStartIndices.toSeq.map(_.getValue))
        }.toVector
      }
    }
    def splitScriptWithStart(
      rawCode: String,
      fileName: String
    ): Either[(Int, String), IndexedSeq[(Int, String, Seq[(Int, String)])]] = {
      val res =
        try Right(parser.scriptBlocksWithStartIndices(rawCode, fileName))
        catch {
          case e: Parser.ScriptSplittingError =>
            Left((e.index, e.expected))
        }
      res.map { blocks =>
        blocks.map { block =>
          (
            block.startIndex,
            block.ncomment,
            block.codeWithStartIndices.toSeq.map(e => (e.getKey: Int, e.getValue))
          )
        }.toVector
      }
    }
  }
}
