package ammonite.repl.tools

import javassist.{ByteArrayClassPath, CtClass, CtMethod}

import ammonite.ops._
import ammonite.repl.Highlighter
import ammonite.repl.Highlighter.{defaultHighlightIndices0, flattenIndices}
import ammonite.runtime.tools.browse.Strings
import ammonite.util.CodeColors
import com.github.javaparser.{GeneratedJavaParserConstants, ParseStart, StringProvider}
import sourcecode.Compat._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.experimental.macros

object source{
  def browseSourceCommand(targetLine: Int) = Seq("less", "+" + targetLine,"-RMN")

  def apply(f: => Any)
           (implicit pprinter: pprint.PPrinter, colors: CodeColors): Unit = macro applyMacro1

  def apply(f: => Any, command: Int => Strings)
           (implicit pprinter: pprint.PPrinter, colors: CodeColors): Unit = macro applyMacro2

  def applyMacro1(c: Context)
                 (f: c.Expr[Any])
                 (pprinter: c.Expr[pprint.PPrinter],
                 colors: c.Expr[CodeColors]): c.Expr[Unit] = {
    import c.universe._
    val defaultBrowseExpr = c.Expr[Int => Strings](
      q"_root_.ammonite.repl.tools.source.browseSourceCommand"
    )
    applyMacro2(c)(f, defaultBrowseExpr)(pprinter, colors)
  }

  def applyMacro2(c: Context)
                 (f: c.Expr[Any], command: c.Expr[Int => Strings])
                 (pprinter: c.Expr[pprint.PPrinter],
                 colors: c.Expr[CodeColors]): c.Expr[Unit] = {
    import c.universe._

    // We don't use quasiquote pattern matching here, because we were seeing
    // weird behavior where the quasiquote q"{..$foo; $bar}" would match single
    // expressions not enclosed in blocks, and recursing on `bar` would result
    // in an infinite recursion. No such problem matching on the `Block` AST node.
    @tailrec def rec(args: Seq[Tree], x: Tree): Option[(Tree,  String, Seq[Tree])] = x match{
      case Select(qualifier, selector) =>
        if (selector.toString == "<init>") None
        else Some(qualifier, selector.toTermName.toString, args)
      case Apply(fun, args) => rec(args, fun)

      case TypeApply(fun, targs) => rec(Nil, fun)
      case Function(vparams, body) => rec(Nil, body)
      case Block(stats, expr) => rec(Nil, expr)
      case _ => None
    }

    val prefix = q"ammonite.repl.tools.source"

    val res = rec(Nil, f.tree) match{
      case Some((lhs, rhs, args)) =>
        val argClasses = args.map{x => q"classOf[${x.tpe}]"}
        q"$prefix.browseObjectMember($lhs, $rhs, $pprinter, $colors, $command, ..$argClasses)"
      case None =>
        q"$prefix.browseObject($f, $pprinter, $colors, $command)"
    }

    c.Expr[Unit](res)
  }

  /**
    * Pull the height from the pretty-printer as a heuristic to shift the
    * desired line towards the middle of the screen. Typically, the
    * pretty-printer's default height is about half the height of the window,
    * so this centers the target line vertically. There is some random
    * variation due to the way we're getting line numbers from bytecode, so
    * hopefully centering it will help ensure the *actual* desired line is
    * visible even if the line number we're aiming for is inaccurate
    */
  def getOffset(p: pprint.PPrinter) = p.defaultHeight

  /**
    * Note: `value` must be a concrete, instantiated value with a concrete class,
    * and cannot be an interface or abstract class. We make use of line numbers
    * from the bytecode to decide which source to show, and those only exist
    * for concrete method implementations
    */
  def browseObject(value: Any,
                   pprinter: pprint.PPrinter,
                   colors: CodeColors,
                   command: Int => Strings) = {
    browseSource(
      loadObjectInfo(value),
      pprinter.defaultHeight,
      colors,
      command
    )
  }

  def loadObjectInfo(value: Any) = {
    loadSource(
      value.getClass,
      _.getMethods.map(_.getMethodInfo.getLineNumber(0)).filter(_ >= 0).min
    ) 
  }
  /**
    * Note: `value` must be a concrete, instantiated value with a concrete class,
    * and cannot be an interface or abstract class. We make use of line numbers
    * from the bytecode to decide which source to show, and those only exist
    * for concrete method implementations
    */
  def browseObjectMember(value: Any,
                         memberName: String,
                         pprinter: pprint.PPrinter,
                         colors: CodeColors,
                         command: Int => Strings,
                         argTypes: Class[_]*) = {
    browseSource(
      loadObjectMemberInfo(value, memberName, argTypes),
      pprinter.defaultHeight,
      colors,
      command
    )
  }

  def getDesc(argTypes: Seq[Class[_]], returnType: Class[_]): String = {
    // https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.3
    def unparse(t: Class[_]): String = {
      t match{
        case t if t.isPrimitive =>
          t.toString match{
            case "void" => "V"
            case "boolean" => "Z"
            case "byte" => "B"
            case "char" => "C"
            case "short" => "S"
            case "int" => "I"
            case "float" => "F"
            case "long" => "J"
            case "double" => "D"
          }
        case t if t.isArray => "[" + unparse(t.getComponentType)
        case t => "L" + t.getName.replace('.', '/') + ";"
      }
    }
    "(" + argTypes.map(unparse).mkString("") + ")" + unparse(returnType)
  }

  def loadObjectMemberInfo(value: Any, memberName: String, argTypes: Seq[Class[_]]) = {
    val method = value.getClass.getMethod(memberName, argTypes:_*)
    loadSource(
      method.getDeclaringClass,
      _.getMethod(memberName, getDesc(argTypes, method.getReturnType))
        .getMethodInfo
        .getLineNumber(0)
    )
  }

  def loadCtClsMetadata(runtimeCls: Class[_], bytecode: Array[Byte]) = {
    val pool = new javassist.ClassPool()
    val cp = new ByteArrayClassPath(runtimeCls.getName, bytecode)
    pool.insertClassPath(cp)
    pool.get(runtimeCls.getName)
  }

  def loadSource(runtimeCls: Class[_], getLineNumber: CtClass => Int) = {
    val chunks = runtimeCls.getName.split('.')
    val (pkg, clsName) = (chunks.init, chunks.last)
    for{
      bytecode <- util.Try{
        read.bytes! resource / pkg / (clsName + ".class")
      }.toEither.left.map { _ =>
        "Unable to find bytecode for class " + runtimeCls.getName
      }
      ctCls = loadCtClsMetadata(runtimeCls, bytecode)

      lineNumber = getLineNumber(ctCls)
      srcFile = ctCls.getClassFile.getSourceFile
      sourceCode <- util.Try{
        read! resource/ pkg / srcFile
      }.toEither.left.map { _ =>
        "Unable to find sourcecode for class " + runtimeCls.getName
      }
    } yield (srcFile, sourceCode, lineNumber)
  }

  def browseSource(loaded: Either[String, (String, String, Int)],
                   verticalOffset: Int,
                   colors: CodeColors,
                   command: Int => Strings) = {

    loaded match{
      case Right((srcFile, sourceCode, lineNumber)) =>
        import ImplicitWd._
        val colored =
          if (srcFile.endsWith(".scala")){
            fansi.Str(
              Highlighter.defaultHighlight0(
                scalaparse.Scala.CompilationUnit,
                sourceCode.toVector,
                colors.comment,
                colors.`type`,
                colors.literal,
                colors.keyword,
                fansi.Attr.Reset
              )
            )
          }else if (srcFile.endsWith(".java")){
            highlightJavaCode(sourceCode, colors)
          }else {
            fansi.Str(sourceCode)
          }
        // Break apart the colored input into lines and then render each line
        // individually

        // We need to do this because `less` and many other paging programs do
        // not properly handle colors spilling across multiple lines
        val plainText = colored.plainText
        val output = mutable.Buffer.empty[String]
        var current = 0
        while({
          val next = plainText.indexOf('\n', current+1)
          if (next != -1) {
            output.append(colored.substring(current, next).render)
            current = next + 1
            true
          }else{
            output.append(colored.substring(current, colored.length).render)
            false
          }
        })()

        val targetLine = lineNumber - verticalOffset
        val tmpFile = tmp(output.mkString("\n"), suffix = "." + srcFile)
        %(command(targetLine).values, tmpFile)
      case Left(msg) => println(msg)
    }
  }

  def highlightJavaCode(sourceCode: String, colors: CodeColors) = {
    import collection.JavaConverters._
    val parsed = new com.github.javaparser.JavaParser().parse(
      ParseStart.COMPILATION_UNIT,
      new StringProvider(sourceCode)
    ).getTokens

    val lineCounts = sourceCode.lines.map(_.length).toArray

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

      fansi.Str(flattenIndices(indices, sourceCode.toVector))
    }
  }

}