package ammonite.repl

import java.net.URLClassLoader

import ammonite.repl.eval.{Evaluator, Preprocessor, Classpath, Compiler}

import scala.reflect.io.VirtualDirectory

/**
 * Created by haoyi on 2/9/15.
 */
class Interpreter(initReplBridge: () => Unit){
  val dynamicClasspath = new VirtualDirectory("(memory)", None)
  var extraJars = Seq[java.io.File]()
  var extraJarClassloaders = Seq[ClassLoader]()

  var compiler: Compiler = _
  def initCompiler() = {
    compiler = new Compiler(
      Classpath.jarDeps ++ extraJars,
      Classpath.dirDeps,
      dynamicClasspath
    )
  }

  initCompiler()


  def loadJar(jar: java.io.File) = {
    extraJars = extraJars ++ Seq(jar)
    extraJarClassloaders ++= Seq(new URLClassLoader(
      Array(jar.toURI.toURL),
      getClass.getClassLoader
    ))
    initCompiler()
    initReplBridge()
  }

  val mainThread = Thread.currentThread()
  val preprocess = new Preprocessor((s) => compiler.parse(s))

  val eval = new Evaluator(
    mainThread.getContextClassLoader,
    extraJarClassloaders,
    preprocess.apply,
    (b, o) => compiler.compile(b, o),
    (w, c) => compiler.importsFor(w, c)
  )
}
