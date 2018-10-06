package ammonite.interp

import ammonite._
import ammonite.util._
import ammonite.util.Util.{CodeSource, newLine, normalizeNewlines, windowsPlatform}
import fastparse.all._

import scala.reflect.internal.Flags
import scala.tools.nsc.{Global => G}
import collection.mutable
/**
  * Responsible for all scala-source-code-munging that happens within the
  * Ammonite REPL.
  *
  * Performs several tasks:
  *
  * - Takes top-level Scala expressions and assigns them to `res{1, 2, 3, ...}`
  *   values so they can be accessed later in the REPL
  *
  * - Wraps the code snippet with an wrapper `object` since Scala doesn't allow
  *   top-level expressions
  *
  * - Mangles imports from our [[ammonite.util.ImportData]] data structure into a source
  *   String
  *
  * - Combines all of these into a complete compilation unit ready to feed into
  *   the Scala compiler
  */
trait Preprocessor{
  def transform(stmts: Seq[String],
                resultIndex: String,
                leadingSpaces: String,
                codeSource: CodeSource,
                indexedWrapperName: Name,
                imports: Imports,
                printerTemplate: String => String,
                extraCode: String,
                skipEmpty: Boolean,
                codeWrapper: Preprocessor.CodeWrapper): Res[Preprocessor.Output]
}

object Preprocessor{
  private case class Expanded(code: String, printer: Seq[String])
  case class Output(code: String,
                    prefixCharLength: Int,
                    userCodeNestingLevel: Int)


  def formatFastparseError(fileName: String, rawCode: String, f: Parsed.Failure) = {
    val lineColIndex = f.extra.input.repr.prettyIndex(f.extra.input, f.index)
    val expected = f.extra.traced.expected
      val locationString = {
        val (first, last) = rawCode.splitAt(f.index)
        val lastSnippet = last.split(newLine).headOption.getOrElse("")
        val firstSnippet = first.reverse
          .split(newLine.reverse)
          .lift(0).getOrElse("").reverse
        firstSnippet + lastSnippet + newLine + (" " * firstSnippet.length) + "^"
      }
    s"$fileName:$lineColIndex expected $expected$newLine$locationString"
  }


  /**
    * Splits up a script file into its constituent blocks, each of which
    * is a tuple of (leading-whitespace, statements). Leading whitespace
    * is returned separately so we can later manipulate the statements e.g.
    * by adding `val res2 = ` without the whitespace getting in the way
    */
  def splitScript(rawCode: String, fileName: String): Res[IndexedSeq[(String, Seq[String])]] = {
    Parsers.splitScript(rawCode) match {
      case f: Parsed.Failure =>
        Res.Failure(formatFastparseError(fileName, rawCode, f))

      case s: Parsed.Success[Seq[(String, Seq[String])]] =>

        var offset = 0
        val blocks = mutable.ArrayBuffer[(String, Seq[String])]()

        // comment holds comments or empty lines above the code which is not caught along with code
        for( (comment, code) <- s.value){

          //ncomment has required number of newLines appended based on OS and offset
          //since fastparse has hardcoded `\n`s, while parsing strings with `\r\n`s it
          //gives out one extra `\r` after '@' i.e. block change
          //which needs to be removed to get correct line number (It adds up one extra line)
          //thats why the `comment.substring(1)` thing is necessary
          val ncomment =
            if(windowsPlatform && blocks.nonEmpty && !comment.isEmpty){
              comment.substring(1) + newLine * offset
            }else{
              comment + newLine * offset
            }

          // 1 is added as Separator parser eats up the newLine char following @
          offset = offset + (comment.split(newLine, -1).length - 1) +
            code.map(_.split(newLine, -1).length - 1).sum + 1
          blocks.append((ncomment, code))
        }

        Res.Success(blocks)
    }
  }

  def apply(parse: => String => Either[String, Seq[G#Tree]]): Preprocessor = new Preprocessor{

    def transform(stmts: Seq[String],
                  resultIndex: String,
                  leadingSpaces: String,
                  codeSource: CodeSource,
                  indexedWrapperName: Name,
                  imports: Imports,
                  printerTemplate: String => String,
                  extraCode: String,
                  skipEmpty: Boolean,
                  codeWrapper: CodeWrapper) = {
      // All code Ammonite compiles must be rooted in some package within
      // the `ammonite` top-level package
      assert(codeSource.pkgName.head == Name("ammonite"))
      for{
        Preprocessor.Expanded(code, printer) <- expandStatements(stmts, resultIndex, skipEmpty)
        (wrappedCode, importsLength, userCodeNestingLevel) = wrapCode(
          codeSource, indexedWrapperName, leadingSpaces + code,
          printerTemplate(printer.mkString(", ")),
          imports, extraCode, codeWrapper
        )
      } yield Preprocessor.Output(wrappedCode, importsLength, userCodeNestingLevel)
    }

    def Processor(cond: PartialFunction[(String, String, G#Tree), Preprocessor.Expanded]) = {
      (code: String, name: String, tree: G#Tree) => cond.lift(name, code, tree)
    }

    def pprintSignature(ident: String, customMsg: Option[String]) = {
      val customCode = customMsg.fold("_root_.scala.None")(x => s"""_root_.scala.Some("$x")""")
      s"""
      _root_.ammonite
            .repl
            .ReplBridge
            .value
            .Internal
            .print($ident, ${fastparse.utils.Utils.literalize(ident)}, $customCode)
      """
    }
    def definedStr(definitionLabel: String, name: String) =
      s"""
      _root_.ammonite
            .repl
            .ReplBridge
            .value
            .Internal
            .printDef("$definitionLabel", ${fastparse.utils.Utils.literalize(name)})
      """

    def pprint(ident: String) = pprintSignature(ident, None)


    /**
     * Processors for declarations which all have the same shape
     */
    def DefProc(definitionLabel: String)(cond: PartialFunction[G#Tree, G#Name]) =
      (code: String, name: String, tree: G#Tree) =>
        cond.lift(tree).map{ name =>
          Preprocessor.Expanded(
            code,
            Seq(definedStr(definitionLabel, Name.backtickWrap(name.decoded)))
          )
        }

    val ObjectDef = DefProc("object"){case m: G#ModuleDef => m.name}
    val ClassDef = DefProc("class"){ case m: G#ClassDef if !m.mods.isTrait => m.name }
    val TraitDef =  DefProc("trait"){ case m: G#ClassDef if m.mods.isTrait => m.name }
    val DefDef = DefProc("function"){ case m: G#DefDef => m.name }
    val TypeDef = DefProc("type"){ case m: G#TypeDef => m.name }

    val PatVarDef = Processor { case (name, code, t: G#ValDef) =>
      Expanded(
        //Only wrap rhs in function if it is not a function
        //Wrapping functions causes type inference errors.
        code,
        // Try to leave out all synthetics; we don't actually have proper
        // synthetic flags right now, because we're dumb-parsing it and not putting
        // it through a full compilation
        if (t.name.decoded.contains("$")) Nil
        else if (!t.mods.hasFlag(Flags.LAZY)) Seq(pprint(Name.backtickWrap(t.name.decoded)))
        else Seq(s"""${pprintSignature(Name.backtickWrap(t.name.decoded), Some("<lazy>"))}""")
      )
    }

    val Import = Processor{
      case (name, code, tree: G#Import) =>
        val Array(keyword, body) = code.split(" ", 2)
        val tq = "\"\"\""
        Expanded(code, Seq(
          s"""
          _root_.ammonite
                .repl
                .ReplBridge
                .value
                .Internal
                .printImport(${fastparse.utils.Utils.literalize(body)})
          """
        ))
    }

    val Expr = Processor{
      //Expressions are lifted to anon function applications so they will be JITed
      case (name, code, tree) => Expanded(s"val $name = $code", Seq(pprint(name)))
    }

    val decls = Seq[(String, String, G#Tree) => Option[Preprocessor.Expanded]](
      ObjectDef, ClassDef, TraitDef, DefDef, TypeDef, PatVarDef, Import, Expr
    )

    def expandStatements(stmts: Seq[String],
                         wrapperIndex: String,
                         skipEmpty: Boolean): Res[Preprocessor.Expanded] = {
      stmts match{
        // In the REPL, we do not process empty inputs at all, to avoid
        // unnecessarily incrementing the command counter
        //
        // But in scripts, we process empty inputs and create an empty object,
        // to ensure that when the time comes to cache/load the class it exists
        case Nil if skipEmpty => Res.Skip
        case postSplit =>
          complete(stmts.mkString(""), wrapperIndex, postSplit)

      }
    }

    def complete(code: String, resultIndex: String, postSplit: Seq[String]) = {
      val reParsed = postSplit.map(p => (parse(p), p))
      val errors = reParsed.collect{case (Left(e), _) => e }
      if (errors.length != 0) Res.Failure(errors.mkString(newLine))
      else {
        val allDecls = for {
          ((Right(trees), code), i) <- reParsed.zipWithIndex if (trees.nonEmpty)
        } yield {
          // Suffix the name of the result variable with the index of
          // the tree if there is more than one statement in this command
          val suffix = if (reParsed.length > 1) "_" + i else ""
          def handleTree(t: G#Tree) = {
            decls.iterator.flatMap(_.apply(code, "res" + resultIndex + suffix, t)).next()
          }
          trees match {
            case Seq(tree) => handleTree(tree)

            // This handles the multi-import case `import a.b, c.d`
            case trees if trees.forall(_.isInstanceOf[G#Import]) => handleTree(trees(0))

            // AFAIK this can only happen for pattern-matching multi-assignment,
            // which for some reason parse into a list of statements. In such a
            // scenario, aggregate all their printers, but only output the code once
            case trees =>
              val printers = for {
                tree <- trees
                if tree.isInstanceOf[G#ValDef]
                Preprocessor.Expanded(_, printers) = handleTree(tree)
                printer <- printers
              } yield printer

              Preprocessor.Expanded(code, printers)
          }
        }

        allDecls match{
          case Seq(first, rest@_*) =>
            val allDeclsWithComments = Expanded(first.code, first.printer) +: rest
            Res(
              allDeclsWithComments.reduceOption { (a, b) =>
                Expanded(
                  // We do not need to separate the code with our own semi-colons
                  // or newlines, as each expanded code snippet itself comes with
                  // it's own trailing newline/semicolons as a result of the
                  // initial split
                  a.code + b.code,
                  a.printer ++ b.printer
                )
              },
              "Don't know how to handle " + code
            )
          case Nil => Res.Success(Expanded("", Nil))
        }
      }
    }
  }



  def wrapCode(codeSource: CodeSource,
               indexedWrapperName: Name,
               code: String,
               printCode: String,
               imports: Imports,
               extraCode: String,
               codeWrapper: CodeWrapper) = {

    //we need to normalize topWrapper and bottomWrapper in order to ensure
    //the snippets always use the platform-specific newLine
    val (topWrapper, bottomWrapper, userCodeNestingLevel) =
     codeWrapper(code, codeSource, imports, printCode, indexedWrapperName, extraCode)
    val importsLen = topWrapper.length

    (topWrapper + code + bottomWrapper, importsLen, userCodeNestingLevel)
  }


  trait CodeWrapper{
    def wrapperPath: Seq[Name] = Nil
    def apply(
      code: String,
      source: CodeSource,
      imports: Imports,
      printCode: String,
      indexedWrapperName: Name,
      extraCode: String
    ): (String, String, Int)
  }
  object CodeWrapper extends CodeWrapper{
    private val userCodeNestingLevel = 1
    def apply(
      code: String,
      source: CodeSource,
      imports: Imports,
      printCode: String,
      indexedWrapperName: Name,
      extraCode: String
    ) = {
      import source.pkgName
      val top = normalizeNewlines(s"""
package ${pkgName.head.encoded}
package ${Util.encodeScalaSourcePath(pkgName.tail)}
$imports

object ${indexedWrapperName.backticked}{\n"""
      )
      val bottom = normalizeNewlines(s"""\ndef $$main() = { $printCode }
  override def toString = "${indexedWrapperName.encoded}"
  $extraCode
}
""")

      (top, bottom, userCodeNestingLevel)
    }
  }
  object CodeClassWrapper extends CodeWrapper{
    /*
     * The goal of this code wrapper is that the user code:
     * - should be in a class rather than a singleton,
     * - should see the previous commands results via instances of these classes,
     *   not referencing singletons along the way.
     *
     * Only dealing with class instances at runtime, rather than singletons, behaves
     * well wrt Java serialization. Singletons don't write their fields during serialization,
     * and re-compute them when deserialized. On the other hand, class instances serialize
     * and de-serialize their fields, as expected.
     *
     * It still allows users to wrap code in singletons rather than a class if they want to:
     * user code that solely consists of a singleton, is itself wrapped in a singleton,
     * rather than a class. This is useful for macro code, or definitions that are
     * themselves processed by macros (definitions in objects are easier to process from
     * macros).
     */
    private val userCodeNestingLevel = 2
    private val q = "\""
    private val tq = "\"\"\""
    override val wrapperPath: Seq[Name] = Seq(Name("instance"))
    def apply(
      code: String,
      source: CodeSource,
      imports: Imports,
      printCode: String,
      indexedWrapperName: Name,
      extraCode: String
    ) = {
      import source.pkgName
      val isObjDef = Parsers.isObjDef(code)

      if (isObjDef) {
        val top = normalizeNewlines(s"""
package ${pkgName.head.encoded}
package ${Util.encodeScalaSourcePath(pkgName.tail)}

$imports

object ${indexedWrapperName.backticked}{
  val instance: Helper.type = Helper
  def $$main() = instance.$$main()

  object Helper extends java.io.Serializable {
"""
        )

        val bottom = normalizeNewlines(s"""\ndef $$main() = { $printCode }
  override def toString = "${indexedWrapperName.encoded}"
  $extraCode
}}
""")

        (top, bottom, userCodeNestingLevel)
      } else {

        val (reworkedImports, reqVals) = {

          val (l, reqVals0) = imports
            .value
            .map { data =>
              val prefix = Seq(Name("_root_"), Name("ammonite"), Name("$sess"))
              if (data.prefix.startsWith(prefix) && data.prefix.endsWith(wrapperPath)) {
                val name = data.prefix.drop(prefix.length).dropRight(wrapperPath.length).last
                (data.copy(prefix = Seq(name)), Seq(name -> data.prefix))
              } else
                (data, Nil)
            }
            .unzip

          (Imports(l), reqVals0.flatten)
        }

        val requiredVals = reqVals
          .distinct
          .groupBy(_._1)
          .mapValues(_.map(_._2))
          .toVector
          .sortBy(_._1.raw)
          .collect {
            case (key, Seq(path)) =>
              /*
               * Via __amm_usedThings, that itself relies on the *-tree.txt resources generated
               * via the AmmonitePlugin, we can know whether the current command uses things from
               * each of the previous ones, and null-ify the references to those that are unused.
               * That way, the unused commands don't prevent serializing this command.
               */
              val encoded = Util.encodeScalaSourcePath(path)
              s"final val ${key.backticked}: $encoded.type = " +
                s"if (__amm_usedThings($tq${key.raw}$tq)) $encoded else null$newLine"
            case (key, values) =>
              throw new Exception(
                "Should not happen - several required values with the same name " +
                  s"(name: $key, values: $values)"
              )
          }
          .mkString

        val usedThingsSet =
          if (reqVals.isEmpty) ""
          else
            s"""
  @_root_.scala.transient private val __amm_usedThings =
    _root_.ammonite.repl.ReplBridge.value.usedEarlierDefinitions.toSet"""

        val top = normalizeNewlines(s"""
package ${pkgName.head.encoded}
package ${Util.encodeScalaSourcePath(pkgName.tail)}

object ${indexedWrapperName.backticked}{
  val wrapper = new ${indexedWrapperName.backticked}
  val instance = new wrapper.Helper
  def $$main() = instance.$$main()
}

final class ${indexedWrapperName.backticked} extends java.io.Serializable {

$usedThingsSet

  override def toString = $q${indexedWrapperName.encoded}$q
$requiredVals
$reworkedImports

final class Helper extends java.io.Serializable{\n"""
    )

        val bottom = normalizeNewlines(s"""\ndef $$main() = { $printCode }
  override def toString = "${indexedWrapperName.encoded}"
  $extraCode
}}
""")

        (top, bottom, userCodeNestingLevel)
      }
    }
  }
}

