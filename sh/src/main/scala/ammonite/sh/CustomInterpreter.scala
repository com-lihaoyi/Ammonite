package ammonite
package sh

import javax.script.{CompiledScript, ScriptContext, ScriptEngine, ScriptException}

import sun.misc.{SignalHandler, Signal}

import scala.tools.nsc.interpreter._
import scala.tools.nsc.util._


class CustomILoop extends ILoop{


  private[this] object Signaller{

    val mainThread = Thread.currentThread()
    val SIGINT = new Signal("INT")
    var oldSigInt: Any = null
    @volatile var currentlyRunning = false
    def handlers = {
      val handlersField = classOf[Signal].getDeclaredField("handlers")
      handlersField.setAccessible(true)
      handlersField.get(null)
        .asInstanceOf[java.util.Hashtable[Signal, SignalHandler]]
    }
    def ifSignalSupported(t: => Unit) = {
      try {
        Class.forName("sun.misc.Signal")
        Class.forName("sun.misc.SignalHandler")

        t
      } catch {case e: ClassNotFoundException=>

      }
    }
    def enable() = ifSignalSupported{
      oldSigInt = handlers.get(SIGINT)
      sun.misc.Signal.handle(SIGINT, new SignalHandler () {
        def handle(sig: Signal) {
          currentlyRunning match{
            case true =>
              mainThread.stop()
              currentlyRunning = false
            case false => println("Ctrl-D to exit")
          }
        }
      })
    }

    def disable() = ifSignalSupported{
      handlers.put(SIGINT, oldSigInt.asInstanceOf[SignalHandler])
    }
  }

  override def closeInterpreter() = {
    Signaller.disable()
  }

  override def createInterpreter()  = {

    Signaller.enable()
    if (addedClasspath != "") settings.classpath append addedClasspath
    intp = new ILoopInterpreter with CustomIMain{
      def running[T](t: => T) = {
        Signaller.currentlyRunning = true
        val res = t
        Signaller.currentlyRunning = false
        res
      }
    }
  }
}

trait CustomIMain extends IMain{ imain =>
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
    private class ObjectBasedWrapper extends Wrapper {
      def preambleHeader = "object %s {"

      def postamble = importsTrailer + "\n}"

      def postwrap = "}\n"
    }

    private class ClassBasedWrapper extends Wrapper {
      def preambleHeader = "class %s extends Serializable {"

      /** Adds an object that instantiates the outer wrapping class. */
      def postamble  = s"""$importsTrailer
                          |}
                          |object ${lineRep.readName} extends ${lineRep.readName}
                          |""".stripMargin
      import nme.{ INTERPRETER_IMPORT_WRAPPER => iw }

      /** Adds a val that instantiates the wrapping class. */
      def postwrap = s"}\nval $iw = new $iw\n"
    }

    private lazy val ObjectSourceCode: Wrapper =
      if (settings.Yreplclassbased) new ClassBasedWrapper else new ObjectBasedWrapper

    lazy val customMemberHandlers = new {
      val intp: imain.type = imain
    } with CustomMemberHandlers

    private object ResultObjectSourceCode extends IMain.CodeAssembler[customMemberHandlers.MemberHandler] {
      /** We only want to generate this code when the result
        *  is a value which can be referred to as-is.
        */
      val evalResult = RequestX.this.value match {
        case NoSymbol => ""
        case sym      => "lazy val %s = %s".format(lineRep.resultName, originalPath(sym))
      }
      // first line evaluates object to make sure constructor is run
      // initial "" so later code can uniformly be: + etc

      val preamble = """
                       |object %s {
                       |  %s
                       |  lazy val %s: String = %s {
                       |    %s
                       |    (""
                     """.stripMargin.format(
          lineRep.evalName, evalResult, lineRep.printName,
          executionWrapper, fullAccessPath
        )

      val postamble = """
                        |    )
                        |  }
                        |}
                      """.stripMargin
      override def apply(contributors: List[customMemberHandlers.MemberHandler]): String = stringFromWriter { code =>
        code println preamble
        contributors map generate foreach (code println _)
        code println postamble
      }
      val generate = (m: customMemberHandlers.MemberHandler) => m resultExtractionCode RequestX.this
    }
    /** Compile the object file.  Returns whether the compilation succeeded.
      *  If all goes well, the "types" map is computed. */
    override lazy val compile: Boolean = {
      // error counting is wrong, hence interpreter may overlook failure - so we reset
      reporter.reset()

      // compile the object containing the user's code
      lineRep.compile(ObjectSourceCode(handlers)) && {
        // extract and remember types
        typeOf
        typesOfDefinedTerms

        // Assign symbols to the original trees
        // TODO - just use the new trees.
        defHandlers foreach { dh =>
          val name = dh.member.name
          definedSymbols get name foreach { sym =>
            dh.member setSymbol sym
          }
        }

        // compile the result-extraction object
        val handls = if (true) trees.map(customMemberHandlers.chooseHandler) else Nil
        withoutWarnings(lineRep compile ResultObjectSourceCode(handls))
      }
    }

  }
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
