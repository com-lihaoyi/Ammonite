package ammonite.repl.tools

import ammonite.repl.Highlighter.flattenIndices
import ammonite.util.CodeColors
import com.github.javaparser.GeneratedJavaParserConstants._
import com.github.javaparser.{GeneratedJavaParserConstants, ParseStart, StringProvider}
import sourcecode.Compat.Context

import scala.collection.mutable

object HighlightJava{

  def highlightJavaCode(sourceCode: String, colors: CodeColors) = {
    import collection.JavaConverters._
    val parsed = new com.github.javaparser.JavaParser().parse(
      ParseStart.COMPILATION_UNIT,
      new StringProvider(sourceCode)
    ).getTokens

    val lineCounts = Predef.augmentString(sourceCode).lines.map(_.length).toArray

    def positionToOffset(p: com.github.javaparser.Position) = {
      lineCounts.iterator.take(p.line - 1).sum + (p.line-1) + (p.column - 1)
    }
    if (!parsed.isPresent) fansi.Str(sourceCode)
    else {
      val indices = mutable.Buffer[(Int, fansi.Attrs)]((0, fansi.Attr.Reset))

      for(token <- parsed.get.asScala){
        import GeneratedJavaParserConstants._

        val colorOpt =
          token.getKind match{
            case INTEGER_LITERAL | LONG_LITERAL | FLOATING_POINT_LITERAL |
                 STRING_LITERAL | TRUE | FALSE | NULL => Some(colors.literal)

            // https://en.wikipedia.org/wiki/List_of_Java_keywords
            case ABSTRACT | ASSERT | BOOLEAN | BREAK | BYTE | CASE |
                 CATCH | CHAR | CLASS | CONST | CONTINUE | 49 /*_DEFAULT*/ |
                 DO | DOUBLE | ELSE | ENUM | EXTENDS | FINAL | FINALLY |
                 FLOAT | FOR | GOTO | IF | IMPLEMENTS | IMPORT | INSTANCEOF |
                 INT | INTERFACE | LONG | NATIVE | NEW | PACKAGE | PRIVATE |
                 PROTECTED | PUBLIC | RETURN | SHORT | STATIC | STRICTFP |
                 SUPER | SWITCH | SYNCHRONIZED | THIS | THROW | THROWS |
                 TRANSIENT | TRY | VOID | VOLATILE | WHILE => Some(colors.keyword)

            case SINGLE_LINE_COMMENT | MULTI_LINE_COMMENT | JAVA_DOC_COMMENT =>
              Some(colors.comment)

            // We do not make use of colors.`type`.
            //
            // This lexer does not give us information about which tokens are
            // part of type signatures, and I'm not quite clever enough to
            // reconstruct that information from the token-stream and AST.
            case _ => None
          }

        for(color <- colorOpt){

          indices.append((
            positionToOffset(token.getRange.begin),
            color
          ))
          indices.append((
            // End is inclusive, rather than exclusive as most other
            // range's "ends" are, so add 1 to make it match others
            positionToOffset(token.getRange.end) + 1,
            fansi.Attr.Reset
          ))
        }

      }
      indices.append((999999999, fansi.Attr.Reset))

      fansi.Str(SeqCharSequence(flattenIndices(indices.toSeq, sourceCode.toVector)))
    }
  }
}