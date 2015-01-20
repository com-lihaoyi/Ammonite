package ammonite.sh2

import java.io.{OutputStream, InputStream, PrintWriter, StringWriter}
import java.lang.reflect.InvocationTargetException

import ammonite.sh2.eval.{Evaluator, Preprocessor, Compiler}
import jline.console.ConsoleReader
import acyclic.file

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.io.VirtualDirectory
import scala.util.Try

class Shell(){
  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  val compiler = new Compiler(dynamicClasspath)
  val mainThread = Thread.currentThread()
  val preprocess = new Preprocessor
  val term = new jline.UnixTerminal()
  term.init()

  val eval = new Evaluator(
    mainThread.getContextClassLoader,
    preprocess.apply,
    compiler.compile
  )

  def action(reader: ConsoleReader): Result[(String, String)] = for {
    _ <- Signaller("INT", () => println("Ctrl-D to Exit"))

    _ <- Catching { case x: Throwable =>
      val sw = new StringWriter()
      x.printStackTrace(new PrintWriter(sw))

      sw.toString + "\n" +
        "Something unexpected went wrong =("
    }

    res <- Option(reader.readLine(Console.MAGENTA + "scala> " + Console.RESET))
                          .map(Result.Success(_)).getOrElse(Result.Exit)
    out <- processLine(res)
  } yield out

  def processLine(line: String) = for {
    _ <- Signaller("INT", () => mainThread.stop())
  } yield eval.processLine(line)


  def run(input: InputStream, output: OutputStream) = {
    val reader = new ConsoleReader(input, output, term)
    @tailrec def loop(): Unit = {
      val r = action(reader)
      eval.update(r)
      r match{
        case Result.Exit => reader.println("Bye!")
        case Result.Success((msg, imports)) =>
          reader.println(msg)
          loop()
        case Result.Failure(msg) =>
          reader.println(Console.RED + msg + Console.RESET)
          loop()
      }
    }
    loop()
  }
}

class Shell2 {
  def run(args: Array[String]) = {
    val shell = new Shell()
    shell.run(System.in, System.out)
  }
}

object Shell2{
  import scala.reflect.runtime.universe._
  def typeString[T: TypeTag](t: => T) = typeOf[T].toString

  val validIdentifier = "([a-zA-Z_][a-zA-Z_0-9]+)".r
  def reprSection(s: String) = {
    if (validIdentifier.findFirstIn(s) == Some(s)){
      PPrint[scala.Symbol](Symbol(s))
    }else{
      PPrint[String](s)
    }
  }
  implicit val pathRepr = PPrint.make[ammonite.Path]{p =>
    ("root" +: p.segments.map(reprSection)).mkString("/")
  }
  implicit val relPathRepr = PPrint.make[ammonite.RelPath]{p =>
    if (p.segments.length == 1 && p.ups == 0) "empty/" + PPrint(p.segments(0))
    else (Seq.fill(p.ups)("up") ++ p.segments.map(reprSection)).mkString("/")
  }
}