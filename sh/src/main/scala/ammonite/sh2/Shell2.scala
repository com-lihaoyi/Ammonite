package ammonite.sh2

import java.io.{PrintWriter, StringWriter}
import java.lang.reflect.InvocationTargetException

import ammonite.sh2.executor.{Preprocessor, Compiler}
import jline.console.ConsoleReader
import acyclic.file

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.io.{VirtualDirectory, AbstractFile}
import scala.util.Try

case class Catching[T](handler: PartialFunction[Throwable, T]) {
  def apply(t: => T): T = try t catch handler

//  def foreach(t: Unit => T): T = apply(t(()))
  def flatMap(t: Unit => Result[T]): Result[T] =
    try{t(())} catch handler.andThen(x => Failure(x.toString))
  def map(t: Unit => T): Result[T] = Success(apply(t(())))
}

class Shell2 {
  implicit class SuperOpt[T](opt: Option[T]){
    def err[V](v: V) = new OrElse(v)
    class OrElse[V](v: V){

    }
  }
  def run(args: Array[String]) = {

    println(args.toList)
    val dynamicClasspath = new VirtualDirectory("(memory)", None)
    val compiler = new Compiler(dynamicClasspath)

    val mainThread = Thread.currentThread()
    val newFileDict = mutable.Map.empty[String, Array[Byte]]
    val currentClassloader = new ClassLoader(mainThread.getContextClassLoader) {
      override def loadClass(name: String) = {
        if (!newFileDict.contains(name))super.loadClass(name)
        else defineClass(name, newFileDict(name), 0, newFileDict(name).length)
      }
    }

    mainThread.setContextClassLoader(currentClassloader)
    val term = new jline.UnixTerminal()
    term.init()
    val reader = new ConsoleReader(System.in, System.out, term)

    def action(): Result[String] = for{
      _ <- Signaller("INT", () => println("Ctrl-D to Exit"))
      _ <- Catching[String]{ case x: Throwable =>
        val sw = new StringWriter()
        x.printStackTrace(new PrintWriter(sw))

        Console.RED + sw.toString + Console.RESET + "\n" +
        "Something unexpected went wrong =("
      }
      res <- Option(reader.readLine(Console.MAGENTA + "scala> " + Console.RESET))
                  .fold[Result[String]](Exit)(Success(_))

      (className, wrapped) <- Result(Preprocessor(res), "Don't recognize that input =/")
      _ <- Signaller("INT", () => mainThread.stop())
      compiled <- Result(Try(compiler.compile(wrapped.getBytes, println)), e => e.toString)
      classFiles <- Result(compiled, "Compilation Failed")
      (cls, method) <- Result(Try {
        for (c <- classFiles) {
          val name = c.name.stripSuffix(".class")
          val output = dynamicClasspath.fileNamed(c.name).output
          output.write(c.toByteArray)
          output.close()
          newFileDict(name) = c.toByteArray
        }

        val cls = Class.forName(className, true, currentClassloader)
        (cls, cls.getDeclaredMethod("$main"))
      }, e => "Failed to load compiled class " + e)
      _ <- Catching{
        case ex: InvocationTargetException
          if ex.getCause.isInstanceOf[ExceptionInInitializerError] =>
        val userEx = ex.getCause .getCause
        val trace =
          userEx
            .getStackTrace
            .takeWhile(x => !(x.getClassName == cls.getName && x.getMethodName == "$main"))
            .mkString("\n")

        Console.RED + userEx.toString + "\n" + trace + Console.RESET
      }
    } yield method.invoke(null).toString


    @tailrec def loop(): Unit = action() match{
      case Exit => reader.println("Bye!")
      case Success(msg) =>
        reader.println(msg)
        loop()
      case Failure(msg) =>
        reader.println(msg)
        loop()
    }
    loop()
  }
}
object Shell2{
  import scala.reflect.runtime.universe._
  def typeString[T: TypeTag](t: => T) = {
    typeOf[T].toString
  }

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
//  def typeString[T](t: T): String = macro typeString0
//  def typeString0[T: c.WeakTypeTag](c: blackbox.Context)(t: c.Expr[T]): c.Expr[String] = {
//    weak
//  }
}