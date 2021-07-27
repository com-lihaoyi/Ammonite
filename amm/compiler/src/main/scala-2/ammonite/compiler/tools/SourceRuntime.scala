package ammonite.compiler.tools

import javassist.{ByteArrayClassPath, CtClass, CtMethod}


import ammonite.compiler.Highlighter
import ammonite.runtime.tools.browse.Strings
import ammonite.util.CodeColors
import ammonite.util.Util.{Location, newLine}
import scala.collection.mutable
import scala.language.experimental.macros

object SourceRuntime{

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
      x => {
        // "Declared" gives us all methods, including private ones. This is
        // *different* from the convention in java.lang.reflect, where
        // "Declared" means "not inherited"
        val firstLines = (x.getDeclaredMethods ++ x.getDeclaredConstructors)
          .map(_.getMethodInfo.getLineNumber(0))
          .filter(_ != -1)

        try Right(firstLines.min)
        catch{ case e: UnsupportedOperationException =>
          Left("Unable to find line number of class " + value.getClass)
        }
      }
    )
  }
  /**
    * Note: `value` must be a concrete, instantiated value with a concrete class,
    * and cannot be an interface or abstract class. We make use of line numbers
    * from the bytecode to decide which source to show, and those only exist
    * for concrete method implementations
    */
  def browseObjectMember(symbolOwnerCls: Class[_],
                         value: Option[Any],
                         memberName: String,
                         pprinter: pprint.PPrinter,
                         colors: CodeColors,
                         command: Int => Strings,
                         returnType: Class[_],
                         argTypes: Class[_]*) = {
    browseSource(
      loadObjectMemberInfo(symbolOwnerCls, value, memberName, returnType, argTypes:_*),
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
    val returnDesc = if (returnType.getName == "scala.Unit") "V" else unparse(returnType)
    "(" + argTypes.map(unparse).mkString("") + ")" + returnDesc
  }

  /**
    * A hacky way to try and find a "good" source location for a function,
    * about as good as we can probably get without a huge amount more effort:
    *
    * - We rely on the bytecode line numbers to locate methods; unfortunately,
    *   this only works for concrete, non-abstract methods! But it's the best
    *   we're going to get short of parsing all the source code ourselves
    *
    * - We look at the class that's the "owner" of the Scala symbol at compile
    *   time. This is based on the static type of the value; this *may* be an
    *   abstract method. If it's concrete, we can use it's bytecode line numbers
    *   to find it and we're done
    *
    * - If it's abstract, we then look at the class that's the java.reflect
    *   DeclaringClass of the value's method, at runtime. This may still not
    *   find the actual location (if the method comes from a trait, it'll
    *   give us the class implementing the trait, rather than the trait itself)
    *   but it gives us another chance at finding the concrete implementation.
    *
    * Again, this means it is important there is a concrete `value` that has
    * the method we're looking for, since we're relying on the bytecode line
    * numbers to find the method, which only exist in concrete methods.
    */
  def loadObjectMemberInfo(symbolOwnerCls: Class[_],
                           value: Option[Any],
                           memberName: String,
                           returnType: Class[_],
                           argTypes: Class[_]*) = {

    val desc = getDesc(argTypes, returnType)

    (loadSourceFrom(symbolOwnerCls, memberName, desc), value) match{
      case (Right(loc), _) if loc.lineNum != -1 => Right(loc)
      case (Left(e1), None) => Left(e1)
      case (Left(e1), Some(v)) =>
        try{
          val concreteCls = v.getClass.getMethod(memberName, argTypes:_*).getDeclaringClass
          loadSourceFrom(concreteCls, memberName, desc)
        }catch{case e: NoSuchMethodException =>
          Left(s"$e1${newLine}Unable to find method${value.getClass.getName}#$memberName")
        }
    }
  }
  def loadSourceFrom(cls: Class[_], memberName: String, desc: String) = {
    loadSource(
      cls,
      x => {
        val lineNum =
          try Right(x.getMethod(memberName, desc).getMethodInfo.getLineNumber(0))
          catch{case e: javassist.NotFoundException => Left(e.getMessage)}

        lineNum match{
          case Left(e) => Left(e)
          case Right(n) if n != -1 => Right(n)
          case _ => Left("Cannot find line number of method " + cls.getName + "#"+ memberName)
        }
      }
    )
  }

  def loadCtClsMetadata(runtimeCls: Class[_], bytecode: Array[Byte]) = {
    val pool = new javassist.ClassPool()
    val cp = new ByteArrayClassPath(runtimeCls.getName, bytecode)
    pool.insertClassPath(cp)
    pool.get(runtimeCls.getName)
  }

  def loadSource(runtimeCls: Class[_],
                 getLineNumber: CtClass => Either[String, Int]): Either[String, Location] = {
    val chunks = runtimeCls.getName.split('.')
    val (pkg, clsName) = (chunks.init, chunks.last)

    for{
      bytecode <- try{
        Right(os.read.bytes(os.resource / pkg / (clsName + ".class"))).right
      }catch{ case e: Throwable =>
        Left("Unable to find bytecode for class " + runtimeCls.getName).right
      }
      // Not sure why `ctCls =` doesn't work, but for some reason the
      // for-comprehension desugaring totally screws it up
      ctCls <- Right(loadCtClsMetadata(runtimeCls, bytecode)).right

      lineNumber <- getLineNumber(ctCls).right
      srcFile <- Right(ctCls.getClassFile.getSourceFile).right
      sourceCode <- try{
        Right(os.read(os.resource/ pkg / srcFile)).right
      }catch{case e: Throwable =>
        Left("Unable to find sourcecode for class " + runtimeCls.getName).right
      }
    } yield Location(srcFile, lineNumber, sourceCode)
  }

  def browseSource(loaded: Either[String, Location],
                   verticalOffset: Int,
                   colors: CodeColors,
                   command: Int => Strings) = {

    loaded match{
      case Right(loc) =>
        val colored =
          if (loc.fileName.endsWith(".scala")){
            fansi.Str(SeqCharSequence(
              Highlighter.defaultHighlight0(
                scalaparse.Scala.CompilationUnit(_),
                loc.fileContent.toVector,
                colors.comment,
                colors.`type`,
                colors.literal,
                colors.keyword,
                fansi.Attr.Reset
              )
            ))
          }else if (loc.fileName.endsWith(".java")){
            HighlightJava.highlightJavaCode(loc.fileContent, colors)
          }else {
            fansi.Str(loc.fileContent)
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

        val targetLine = math.max(0, loc.lineNum - verticalOffset)
        val tmpFile = os.temp(output.mkString("\n"), suffix = "." + loc.fileName)
        os.proc(command(targetLine).values, tmpFile)
		  .call(stdin=os.Inherit, stdout=os.Inherit, stderr=os.Inherit)
      case Left(msg) => println(msg)
    }
  }

  def failLoudly[T](res: Either[String, T]): T = res match{
    case Left(s) => throw new Exception(s)
    case Right(r) => r
  }

  def desugarImpl(s: String)(implicit colors: ammonite.util.CodeColors): Desugared = {

    new Desugared(
      ammonite.compiler.Parsers.defaultHighlight(
        s.toVector,
        colors.comment,
        colors.`type`,
        colors.literal,
        colors.keyword,
        colors.error,
        fansi.Attr.Reset
      ).mkString
    )
  }

}
