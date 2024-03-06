package ammonite.compiler

// Originally adapted from
// https://github.com/lampepfl/dotty/blob/3.0.0-M3/
//   compiler/src/dotty/tools/dotc/printing/SyntaxHighlighting.scala

import dotty.tools.dotc
import dotc.CompilationUnit
import dotc.ast.untpd
import dotc.core.Contexts._
import dotc.core.StdNames._
import dotc.parsing.Parsers.Parser
import dotc.parsing.Scanners.Scanner
import dotc.parsing.Tokens._
import dotc.reporting.Reporter
import dotc.util.Spans.Span
import dotc.util.SourceFile

import java.util.Arrays

/** This object provides functions for syntax highlighting in the REPL */
class SyntaxHighlighting(
                          noAttrs: fansi.Attrs,
                          commentAttrs: fansi.Attrs,
                          keywordAttrs: fansi.Attrs,
                          valDefAttrs: fansi.Attrs,
                          literalAttrs: fansi.Attrs,
                          typeAttrs: fansi.Attrs,
                          annotationAttrs: fansi.Attrs,
                          notImplementedAttrs: fansi.Attrs
                        ) {

  def highlight(in: String)(using Context): String = {
    def freshCtx = ctx.fresh.setReporter(Reporter.NoReporter)
    if (in.isEmpty || ctx.settings.color.value == "never") in
    else {
      val source = SourceFile.virtual("<highlighting>", in)

      given Context = freshCtx
        .setCompilationUnit(CompilationUnit(source, mustExist = false)(using freshCtx))

      val colors = Array.fill(in.length)(0L)

      def highlightRange(from: Int, to: Int, attr: fansi.Attrs) =
        Arrays.fill(colors, from, to, attr.applyMask)

      def highlightPosition(span: Span, attr: fansi.Attrs) =
        if (span.exists && span.start >= 0 && span.end <= in.length)
          highlightRange(span.start, span.end, attr)

      val scanner = new Scanner(source)
      while (scanner.token != EOF) {
        val start = scanner.offset
        val token = scanner.token
        val name = scanner.name
        val isSoftModifier = scanner.isSoftModifierInModifierPosition
        scanner.nextToken()
        val end = scanner.lastOffset

        // Branch order is important. For example,
        // `true` is at the same time a keyword and a literal
        token match {
          case _ if literalTokens.contains(token) =>
            highlightRange(start, end, literalAttrs)

          case STRINGPART =>
            // String interpolation parts include `$` but
            // we don't highlight it, hence the `-1`
            highlightRange(start, end - 1, literalAttrs)

          case _ if alphaKeywords.contains(token) || isSoftModifier =>
            highlightRange(start, end, keywordAttrs)

          case IDENTIFIER if name == nme.??? =>
            highlightRange(start, end, notImplementedAttrs)

          case _ =>
        }
      }

      for {
        comment <- scanner.comments
        span = comment.span
      } highlightPosition(span, commentAttrs)

      object TreeHighlighter extends untpd.UntypedTreeTraverser {
        import untpd._

        def ignored(tree: NameTree) = {
          val name = tree.name.toTermName
          // trees named <error> and <init> have weird positions
          name == nme.ERROR || name == nme.CONSTRUCTOR
        }

        def highlightAnnotations(tree: MemberDef): Unit =
          for (annotation <- tree.mods.annotations)
            highlightPosition(annotation.span, annotationAttrs)

        def highlight(trees: List[Tree])(using Context): Unit =
          trees.foreach(traverse)

        def traverse(tree: Tree)(using Context): Unit = {
          tree match {
            case tree: NameTree if ignored(tree) =>
              ()
            case tree: ValOrDefDef =>
              highlightAnnotations(tree)
              highlightPosition(tree.nameSpan, valDefAttrs)
            case tree: MemberDef /* ModuleDef | TypeDef */ =>
              highlightAnnotations(tree)
              highlightPosition(tree.nameSpan, typeAttrs)
            case tree: Ident if tree.isType =>
              highlightPosition(tree.span, typeAttrs)
            case _: TypTree =>
              highlightPosition(tree.span, typeAttrs)
            case _ =>
          }
          traverseChildren(tree)
        }
      }

      val parser = new DottyParser(source)
      val trees = parser.blockStatSeq()
      TreeHighlighter.highlight(trees)

      // if (colorAt.last != NoColor)
      //   highlighted.append(NoColor)

      fansi.Str.fromArrays(in.toCharArray, colors).render
    }
  }
}
