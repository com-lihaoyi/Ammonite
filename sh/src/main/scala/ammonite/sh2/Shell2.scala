package ammonite.sh2

import ammonite.sh2.executor.{Preprocessor, Compiler}
import jline.console.ConsoleReader
import acyclic.file

import scala.collection.mutable
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.reflect.io.{VirtualDirectory, AbstractFile}
import scala.reflect.macros.blackbox
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.util.ClassPath.JavaContext
import scala.tools.nsc.util.{JavaClassPath, DirectoryClassPath}

class Shell2 {

  def run(args: Array[String]) = {

    println(args.toList)
    val dynamicClasspath = new VirtualDirectory("(memory)", None)
    val compiler = new Compiler(dynamicClasspath)

    val mainThread = Thread.currentThread()
    val newFileDict = mutable.Map.empty[String, Array[Byte]]
    val currentClassloader = new ClassLoader(mainThread.getContextClassLoader) {
      override def loadClass(name: String) = {
        if (newFileDict.contains(name)){
          defineClass(name, newFileDict(name), 0, newFileDict(name).length)
        }else{
          super.loadClass(name)
        }
      }
    }

    mainThread.setContextClassLoader(currentClassloader)

    Signaller("INT", println("Ctrl-D to Exit")){
      val term = new jline.UnixTerminal()
      term.init()
      val reader = new ConsoleReader(System.in, System.out, term)

      def loop(): Unit = {
        val res = reader.readLine(Console.MAGENTA + "scala> " + Console.RESET)

        if (res != null){
          Signaller("INT", mainThread.stop()) {
            for {
              (className, wrapped) <- Preprocessor.apply(res)
              compiled = compiler.compile(wrapped.getBytes, println)
              classFiles <- compiled
            } {
              for (c <- classFiles) {
                val name = c.name.stripSuffix(".class")
                val output = dynamicClasspath.fileNamed(c.name).output
                output.write(c.toByteArray)
                output.close()
                newFileDict(name) = c.toByteArray
              }
              Class.forName(className, true, currentClassloader)
                .getDeclaredMethod("$main").invoke(null)
            }
          }
          loop()
        }
      }
      loop()

    }
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