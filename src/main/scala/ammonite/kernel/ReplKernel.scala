package ammonite.kernel

import kernel._
import scalaz.{Name => _, _}
import Scalaz._
import Validation.FlatMap._
import fastparse.core.{Parsed, ParseError}
import scala.tools.nsc.Settings
import scala.reflect.io.VirtualDirectory
import java.io.File
import collection.mutable
import annotation.tailrec
import java.net.URLClassLoader

final class ReplKernel private (private[this] var state: ReplKernel.KernelState) {

  private[this] val lock = new AnyRef

  def process(code: String): KernelOutput = lock.synchronized {

    // type signatures have been included below for documentation

    val parsed: Option[Validation[LogError, NonEmptyList[String]]] = Parsers.Splitter.parse(code) match {
      case Parsed.Success(statements, _) =>
        statements.toList match {
          case h :: t =>
            val nel = NonEmptyList(h, t: _*)
            Some(Validation.success(nel))
          case Nil => None
        }
      case Parsed.Failure(_, index, extra) =>
        Some(Validation.failure(LogError(ParseError.msg(extra.input, extra.traced.expected, index))))
    }

    val res = parsed map { validation =>
      val validationNel = validation.toValidationNel

      validationNel flatMap { statements =>
        val indexedWrapperName = Name(s"cmd${state.evaluationIndex}")
        val wrapperName = Seq(Name("$sess"), indexedWrapperName)

        val munged: ValidationNel[LogError, MungedOutput] = Munger(
          state.compiler.parse,
          statements,
          s"${state.evaluationIndex}",
          Seq(Name("$sess")),
          indexedWrapperName,
          state.imports
        )

        munged flatMap { processed =>
          val compilationResult = state.compiler.compile(processed.code.getBytes,
                                                         processed.prefixCharLength,
                                                         s"_ReplKernel${state.evaluationIndex}.sc")

          compilationResult flatMap {
            case (logMessages, classFiles, imports) =>
              val loadedClass: Validation[LogError, Class[_]] = Validation.fromTryCatchNonFatal {
                for ((name, bytes) <- classFiles.sortBy(_._1)) {
                  state.frame.classloader.addClassFile(name, bytes)
                }
                Class.forName("$sess." + indexedWrapperName.backticked, true, state.frame.classloader)
              } leftMap (LogMessage.fromThrowable(_))

              val processed: Validation[LogError, (Imports, Any)] = loadedClass flatMap { cls =>
                val evaluated: Validation[LogError, Any] = Validation.fromTryCatchNonFatal {
                  Option(cls.getDeclaredMethod(s"$generatedMain").invoke(null)).getOrElse(())
                } leftMap (LogMessage.fromThrowable(_))

                val newImports = Imports(
                  for (id <- imports.value) yield {
                    val filledPrefix =
                      if (id.prefix.isEmpty) {
                        wrapperName
                      } else {
                        id.prefix
                      }
                    val rootedPrefix: Seq[Name] =
                      if (filledPrefix.headOption.exists(_.backticked == "_root_")) {
                        filledPrefix
                      } else {
                        Seq(Name("_root_")) ++ filledPrefix
                      }

                    id.copy(prefix = rootedPrefix)
                  }
                )
                evaluated map ((newImports, _))
              }

              val mapped = processed map (x => (logMessages, x._1, x._2))

              mapped.toValidationNel
          }
        }

      } leftMap (_.reverse)
    }

    // state mutation
    val op = res map { validation =>
      validation map {
        case (logMessages, newImports, value) =>
          state = state.copy(evaluationIndex = state.evaluationIndex + 1, imports = state.imports ++ newImports)
          (logMessages, value)
      }
    }
    op
  }

  def complete(text: String, position: Int): (Int, Seq[String], Seq[String]) = lock.synchronized {
    state.pressy.complete(text, position, Munger.importBlock(state.imports))
  }

}

object ReplKernel {

  private class Frame(val classloader: AmmoniteClassLoader, private[this] var classpath0: Seq[File]) {
    def classpath: Seq[File] = classpath0
    def addClasspath(additional: Seq[File]): Unit = {
      additional.map(_.toURI.toURL).foreach(classloader.add)
      classpath0 = classpath0 ++ additional
    }
  }

  private case class KernelState(evaluationIndex: Int,
                                 frame: Frame,
                                 imports: Imports,
                                 compiler: Compiler,
                                 pressy: Pressy)

  def apply(settings: Settings = new Settings()): ReplKernel = {

    val currentClassLoader = Thread.currentThread().getContextClassLoader
    val hash = AmmoniteClassLoader.initialClasspathSignature(currentClassLoader)
    def special = new AmmoniteClassLoader(currentClassLoader, hash)

    val initialClasspath: List[File] = {
      val res = mutable.ListBuffer[File]()
      res.appendAll(
        System.getProperty("sun.boot.class.path").split(java.io.File.pathSeparator).map(new java.io.File(_))
      )

      @tailrec
      def go(classLoader: ClassLoader): Unit =
        if (classLoader == null) {
          ()
        } else {
          classLoader match {
            case t: URLClassLoader =>
              res.appendAll(t.getURLs.map(u => new File(u.toURI)))
            case _ => ()
          }
          go(classLoader.getParent)
        }

      go(currentClassLoader)

      res.toList.filter(_.exists)
    }

    val dynamicClasspath = new VirtualDirectory("(memory)", None)

    val compiler = new Compiler(
      initialClasspath,
      dynamicClasspath,
      special,
      special,
      settings.copy()
    )

    val pressy = Pressy(
      initialClasspath,
      dynamicClasspath,
      special,
      settings.copy()
    )

    new ReplKernel(KernelState(0, new Frame(special, Seq()), Imports(), compiler, pressy))
  }

}
