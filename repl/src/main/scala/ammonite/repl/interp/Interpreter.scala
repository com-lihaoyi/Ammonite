package ammonite.repl.interp

import java.net.URLClassLoader

import ammonite.repl.Result
import ammonite.repl.frontend.{ReplAPI, ReplAPIHolder}

import scala.reflect.io.VirtualDirectory

class Interpreter(replApi: ReplAPI){
  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  var extraJars = Seq[java.io.File]()
  var extraJarClassloaders = Seq[ClassLoader]()

  var compiler: Compiler = _

  def init() = {
    compiler = new Compiler(
      Classpath.jarDeps ++ extraJars,
      Classpath.dirDeps,
      dynamicClasspath
    )
    compiler.importsFor("", eval.replBridgeCode)
    val cls = eval.evalClass(eval.replBridgeCode, "ReplBridge")
    ReplAPI.initReplBridge(
      cls.asInstanceOf[Result.Success[Class[ReplAPIHolder]]].s,
      replApi
    )
  }

  def loadJar(jar: java.io.File): Unit = {
    extraJars = extraJars ++ Seq(jar)
    extraJarClassloaders ++= Seq(new URLClassLoader(
      Array(jar.toURI.toURL),
      getClass.getClassLoader
    ))
    init()
  }

  val mainThread = Thread.currentThread()
  val preprocess = new Preprocessor(compiler.parse)

  val eval = new Evaluator(
    mainThread.getContextClassLoader,
    extraJarClassloaders,
    preprocess.apply,
    compiler.compile,
    compiler.importsFor
  )
  init()
}
