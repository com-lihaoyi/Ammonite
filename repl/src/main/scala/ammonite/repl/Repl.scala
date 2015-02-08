package ammonite.repl

import java.io.{OutputStream, InputStream}
import ammonite.repl.eval.{Evaluator, Preprocessor, Compiler}
import ammonite.repl.frontend.{DefaultReplAPI, ReplAPIHolder, ReplAPI}
import acyclic.file

import scala.annotation.tailrec
import scala.reflect.io.VirtualDirectory

class Repl(input: InputStream, output: OutputStream) {
  
  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  val compiler = new Compiler(dynamicClasspath)
  val mainThread = Thread.currentThread()
  val preprocess = new Preprocessor


  val eval = new Evaluator(
    mainThread.getContextClassLoader,
    preprocess.apply,
    compiler.compile,
    compiler.importsFor
  )
  val jlineThings = new frontend.JLineThings(
    input,
    output,
    replAPI.shellPrompt,
    eval.previousImportBlock,
    compiler.complete,
    () => mainThread.stop()
  )


  compiler.importsFor("", eval.replBridgeCode)
  val cls = eval.evalClass(eval.replBridgeCode, "ReplBridge")

  lazy val replAPI: ReplAPI = new DefaultReplAPI({
    jlineThings.history
  })

  Repl.initReplBridge(
    cls.asInstanceOf[Result.Success[Class[ReplAPIHolder]]].s,
    replAPI
  )


  def run() = {
    @tailrec def loop(): Unit = {

      val r = for{
        res <- jlineThings.action()
        _ <- Signaller("INT") { mainThread.stop() }
        out <- eval.processLine(res)
      } yield out

      jlineThings.update(r)
      eval.update(r)
      r match{
        case Result.Exit =>
          compiler.pressy.askShutdown()
          println("Bye!")
        case Result.Buffer(line) =>
          loop()
        case Result.Success(ev) =>
          println(ev.msg)
          loop()
        case Result.Failure(msg) =>
          println(Console.RED + msg + Console.RESET)
          loop()
      }
    }
    loop()
  }
}

object Repl{
  def initReplBridge(holder: Class[ReplAPIHolder], api: ReplAPI) = {
    holder
      .getDeclaredMethods
      .find(_.getName.contains('$'))
      .get
      .invoke(null, api)
  }
  def main(args: Array[String]) = {
    val shell = new Repl(System.in, System.out)
    shell.run()
  }
}