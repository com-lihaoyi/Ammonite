package ammonite

// Same as
// https://github.com/lihaoyi/mill/blob/2cda5dcd/scalalib/src/TestRunner.scala
// except we tweak the call to inprocess, so that it uses a different parent class loader.

import java.io.File
import java.net.{URL, URLClassLoader}

import ammonite.util.Colors
import io.github.retronym.java9rtexport.Export
import mill.Agg
import mill.api.ClassLoader.java9OrAbove
import mill.scalalib.Lib.discoverTests
import mill.util.{Ctx, PrintLogger}
import mill.util.JsonFormatters._
import sbt.testing._

import scala.collection.mutable
object TestRunner {

  // See mill.api.makeUrls (private)
  def makeUrls(urls: Array[URL])(implicit ctx: Ctx.Home): Array[URL] = {
    if (java9OrAbove) {
      urls :+ Export.rtAt(ctx.home.toIO).toURI.toURL
    } else {
      urls
    }
  }

  def main(args: Array[String]): Unit = {
    try{
      var i = 0
      def readArray() = {
        val count = args(i).toInt
        val slice = args.slice(i + 1, i + count + 1)
        i = i + count + 1
        slice
      }
      val frameworks = readArray()
      val classpath = readArray()
      val arguments = readArray()
      val outputPath = args(i + 0)
      val colored = args(i + 1)
      val testCp = args(i + 2)
      val homeStr = args(i + 3)
      val ctx = new Ctx.Log with Ctx.Home {
        val log = PrintLogger(
          colored == "true",
          true,
          if(colored == "true") Colors.Default
          else Colors.BlackWhite,
          System.out,
          System.err,
          System.err,
          System.in,
          debugEnabled = false
        )
        val home = os.Path(homeStr)
      }
      val parentLoader = sys.props.get("test.base.classpath").map { cp =>
        val urls = cp.split(File.pathSeparatorChar).map(new File(_).toURI.toURL)
        new URLClassLoader(makeUrls(urls)(ctx), Option(ClassLoader.getSystemClassLoader)
          .flatMap(cl => Option(cl.getParent)).orNull)
      }
      val result = runTests(
        frameworkInstances = TestRunner.frameworks(frameworks),
        entireClasspath = Agg.from(classpath.map(os.Path(_))),
        testClassfilePath = Agg(os.Path(testCp)),
        parentLoader = parentLoader.orNull,
        args = arguments
      )(ctx)

      // Clear interrupted state in case some badly-behaved test suite
      // dirtied the thread-interrupted flag and forgot to clean up. Otherwise
      // that flag causes writing the results to disk to fail
      Thread.interrupted()
      ammonite.ops.write(os.Path(outputPath), upickle.default.write(result))
    }catch{case e: Throwable =>
      println(e)
      e.printStackTrace()
    }
    // Tests are over, kill the JVM whether or not anyone's threads are still running
    // Always return 0, even if tests fail. The caller can pick up the detailed test
    // results from the outputPath
    System.exit(0)
  }

  def inprocess[T](classPath: Agg[os.Path],
                   classLoaderOverrideSbtTesting: Boolean,
                   isolated: Boolean,
                   closeContextClassLoaderWhenDone: Boolean,
                   parentLoader: ClassLoader,
                   body: ClassLoader => T)
                  (implicit ctx: Ctx.Home): T = {
    val urls = classPath.map(_.toIO.toURI.toURL)
    val cl = if (classLoaderOverrideSbtTesting) {
      val outerClassLoader = getClass.getClassLoader
      mill.api.ClassLoader.create(urls.toVector, parentLoader, customFindClass = { name =>
        if (name.startsWith("sbt.testing."))
          Some(outerClassLoader.loadClass(name))
        else None
      })
    } else if (isolated) {
      mill.api.ClassLoader.create(urls.toVector, parentLoader)
    } else {
      mill.api.ClassLoader.create(urls.toVector, getClass.getClassLoader)
    }

    val oldCl = Thread.currentThread().getContextClassLoader
    Thread.currentThread().setContextClassLoader(cl)
    try {
      body(cl)
    } finally {
      if (closeContextClassLoaderWhenDone) {
        Thread.currentThread().setContextClassLoader(oldCl)
        cl.close()
      }
    }
  }

  def runTests(frameworkInstances: ClassLoader => Seq[sbt.testing.Framework],
               entireClasspath: Agg[os.Path],
               testClassfilePath: Agg[os.Path],
               parentLoader: ClassLoader,
               args: Seq[String]
              )(implicit
               ctx: Ctx.Log with Ctx.Home
              ): (String, Seq[mill.scalalib.TestRunner.Result]) = {
    //Leave the context class loader set and open so that shutdown hooks can access it
    inprocess(
      entireClasspath,
      classLoaderOverrideSbtTesting = true,
      isolated = true,
      closeContextClassLoaderWhenDone = false,
      parentLoader,
      cl => {
        val frameworks = frameworkInstances(cl)

        val events = mutable.Buffer.empty[Event]

        val doneMessages = frameworks.map{ framework =>
          val runner = framework.runner(args.toArray, Array[String](), cl)

          val testClasses = discoverTests(cl, framework, testClassfilePath)

          val tasks = runner.tasks(
            for ((cls, fingerprint) <- testClasses.toArray)
              yield new TaskDef(
                cls.getName.stripSuffix("$"),
                fingerprint,
                true,
                Array(new SuiteSelector)
              )
          )

          val taskQueue = tasks.to[mutable.Queue]
          while (taskQueue.nonEmpty){
            val next = taskQueue.dequeue().execute(
              new EventHandler {
                def handle(event: Event) = events.append(event)
              },
              Array(
                new Logger {
                  def debug(msg: String) = ctx.log.outputStream.println(msg)

                  def error(msg: String) = ctx.log.outputStream.println(msg)

                  def ansiCodesSupported() = true

                  def warn(msg: String) = ctx.log.outputStream.println(msg)

                  def trace(t: Throwable) = t.printStackTrace(ctx.log.outputStream)

                  def info(msg: String) = ctx.log.outputStream.println(msg)
                })
            )
            taskQueue.enqueue(next:_*)
          }
          runner.done()
        }

        val results = for(e <- events) yield {
          val ex = if (e.throwable().isDefined) Some(e.throwable().get) else None
          mill.scalalib.TestRunner.Result(
            e.fullyQualifiedName(),
            e.selector() match{
              case s: NestedSuiteSelector => s.suiteId()
              case s: NestedTestSelector => s.suiteId() + "." + s.testName()
              case s: SuiteSelector => s.toString
              case s: TestSelector => s.testName()
              case s: TestWildcardSelector => s.testWildcard()
            },
            e.duration(),
            e.status().toString,
            ex.map(_.getClass.getName),
            ex.map(_.getMessage),
            ex.map(_.getStackTrace)
          )
        }

        (doneMessages.mkString("\n"), results)
      }
    )
  }

  def frameworks(frameworkNames: Seq[String])(cl: ClassLoader): Seq[sbt.testing.Framework] = {
    frameworkNames.map { name =>
      cl.loadClass(name).newInstance().asInstanceOf[sbt.testing.Framework]
    }
  }

  case class Result(fullyQualifiedName: String,
                    selector: String,
                    duration: Long,
                    status: String,
                    exceptionName: Option[String] = None,
                    exceptionMsg: Option[String] = None,
                    exceptionTrace: Option[Seq[StackTraceElement]] = None)

  object Result{
    implicit def resultRW: upickle.default.ReadWriter[Result] = upickle.default.macroRW[Result]
  }
}
