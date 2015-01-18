package ammonite
package sh

import javax.script.{CompiledScript, ScriptContext, ScriptEngine, ScriptException}

import scala.tools.nsc.interpreter._

class CustomILoop extends ILoop{
  @volatile var currentlyRunning = false
  override def createInterpreter() {
    if (addedClasspath != "") settings.classpath append addedClasspath
    intp = new ILoopInterpreter with CustomIMain{
      def running[T](t: => T) = {
        currentlyRunning = true
        val res = t
        currentlyRunning = false
        res
      }
    }
  }
}

trait CustomIMain extends IMain{
  import formatting._
  import reporter.{printMessage, printUntruncatedMessage}
  import global._
  import naming._
  private class WrappedRequest(val req: Request) extends CompiledScript {
    var recorded = false

    /** In Java we would have to wrap any checked exception in the declared
      *  ScriptException. Runtime exceptions and errors would be ok and would
      *  not need to be caught. So let us do the same in Scala : catch and
      *  wrap any checked exception, and let runtime exceptions and errors
      *  escape. We could have wrapped runtime exceptions just like other
      *  exceptions in ScriptException, this is a choice.
      */
    @throws[ScriptException]
    def eval(context: ScriptContext): Object = {
      val result = req.lineRep.evalEither match {
        case Left(e: RuntimeException) => throw e
        case Left(e: Exception) => throw new ScriptException(e)
        case Left(e) => throw e
        case Right(result) => result.asInstanceOf[Object]
      }
      if (!recorded) {
        recordRequest(req)
        recorded = true
      }
      result
    }

    def loadAndRunReq = classLoader.asContext {
      val (result, succeeded) = req.loadAndRun

      /** To our displeasure, ConsoleReporter offers only printMessage,
        *  which tacks a newline on the end.  Since that breaks all the
        *  output checking, we have to take one off to balance.
        */
      if (succeeded) {
        if (true && result != "")
          printMessage(result stripSuffix "\n")
        else if (isReplDebug) // show quiet-mode activity
          printMessage(result.trim.lines map ("[quiet] " + _) mkString "\n")

        // Book-keeping.  Have to record synthetic requests too,
        // as they may have been issued for information, e.g. :type
        recordRequest(req)
        IR.Success
      }
      else {
        // don't truncate stack traces
        printUntruncatedMessage(result)
        IR.Error
      }
    }

    def getEngine: ScriptEngine = CustomIMain.this
  }
  private def compile(line: String, synthetic: Boolean): Either[IR.Result, Request] = {
    if (global == null) Left(IR.Error)
    else requestFromLine(line, synthetic) match {
      case Left(result) => Left(result)
      case Right(req)   =>
        // null indicates a disallowed statement type; otherwise compile and
        // fail if false (implying e.g. a type error)
        if (req == null || !req.compile) Left(IR.Error) else Right(req)
    }
  }
  override def interpret(line: String, synthetic: Boolean): IR.Result = compile(line, synthetic) match {
    case Left(result) => result
    case Right(req)   => new WrappedRequest(req).loadAndRunReq
  }

  override def compiled(script: String): CompiledScript = {
    if (!bound) {
      quietBind("engine" -> this.asInstanceOf[ScriptEngine])
      bound = true
    }
    val cat = code + script
    compile(cat, false) match {
      case Left(result) => result match {
        case IR.Incomplete => {
          code = cat + "\n"
          new CompiledScript {
            def eval(context: ScriptContext): Object = null
            def getEngine: ScriptEngine = CustomIMain.this
          }
        }
        case _ => {
          code = ""
          throw new ScriptException("compile-time error")
        }
      }
      case Right(req)   => {
        code = ""
        new WrappedRequest(req)
      }
    }
  }
  private def earliestPosition(tree: Tree): Int = {
    var pos = Int.MaxValue
    tree foreach { t =>
      pos = math.min(pos, safePos(t, Int.MaxValue))
    }
    pos
  }
  private def safePos(t: Tree, alt: Int): Int =
    try t.pos.start
    catch { case _: UnsupportedOperationException => alt }


  def printing[T](t: T): T = {println(t); t}
  private def requestFromLine(line: String, synthetic: Boolean): Either[IR.Result, Request] = {
    val content = indentCode(line)
    val trees = parse(content) match {
      case parse.Incomplete     => return Left(IR.Incomplete)
      case parse.Error          => return Left(IR.Error)
      case parse.Success(trees) => trees
    }
    // If the last tree is a bare expression, pinpoint where it begins using the
    // AST node position and snap the line off there.  Rewrite the code embodied
    // by the last tree as a ValDef instead, so we can access the value.
    val last = trees.lastOption.getOrElse(EmptyTree)
    last match {
      case _:Assign                        => // we don't want to include assignments
      case _:TermTree | _:Ident | _:Select => // ... but do want other unnamed terms.
        val varName  = if (synthetic) freshInternalVarName() else freshUserVarName()
        val rewrittenLine = {
          // In theory this would come out the same without the 1-specific test, but
          // it's a cushion against any more sneaky parse-tree position vs. code mismatches:
          // this way such issues will only arise on multiple-statement repl input lines,
          // which most people don't use.
          if (trees.size == 1) "val " + varName + " =\n" + content
          else {
            // The position of the last tree
            val lastpos0 = earliestPosition(last)
            // Oh boy, the parser throws away parens so "(2+2)" is mispositioned,
            // with increasingly hard to decipher positions as we move on to "() => 5",
            // (x: Int) => x + 1, and more.  So I abandon attempts to finesse and just
            // look for semicolons and newlines, which I'm sure is also buggy.
            val (raw1, raw2) = content splitAt lastpos0

            val adjustment = (raw1.reverse takeWhile (ch => (ch != ';') && (ch != '\n'))).size
            val lastpos = lastpos0 - adjustment

            // the source code split at the laboriously determined position.
            val (l1, l2) = content splitAt lastpos

            val prefix = if (l1.trim == "") "" else l1 + ";\n"
            // Note to self: val source needs to have this precise structure so that
            // error messages print the user-submitted part without the "val res0 = " part.
            val combined = prefix + "val " + varName + " =\n" + l2

            combined
          }
        }
        // Rewriting    "foo ; bar ; 123"
        // to           "foo ; bar ; val resXX = 123"
        requestFromLine(rewrittenLine, synthetic) match {
          case Right(req) => return Right(req withOriginalLine line)
          case x          => return x
        }
      case _ =>
    }
    Right(new RequestX(line, trees))
  }
  class RequestX(line: String, trees: List[Tree]) extends Request(line, trees){
    override val lineRep = new ReadEvalPrintX(freshLineId())
  }
  var runningThread: Thread = null
  def running[T](t: => T): T

  class ReadEvalPrintX(lineId: Int) extends ReadEvalPrint(lineId){
    def evalMethod(name: String) = evalClass.getMethods filter (_.getName == name) match {
      case Array()       => null
      case Array(method) => method
      case xs            => sys.error("Internal error: eval object " + evalClass + ", " + xs.mkString("\n", "\n", ""))
    }
    override def call(name: String, args: Any*): AnyRef = {
      val m = evalMethod(name)
      running {
        m.invoke(evalClass, args.map(_.asInstanceOf[AnyRef]): _*)
      }

    }
  }
}
