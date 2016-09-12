package ammonite.kernel

import ammonite.runtime._
import kernel._
import scalaz.{Name => _, _}
import Scalaz._
import Validation.FlatMap._
import ammonite.runtime.Parsers
import fastparse.core.{Parsed, ParseError}
import ammonite.util.Imports
import scala.tools.nsc.Settings
import scala.reflect.io.VirtualDirectory
import ammonite.util.{Name, Evaluated}

class ReplKernel private (private[this] var state: ReplKernel.KernelState) {

  def process(code: String): KernelOutput = {

    val evaluationIndex = state.evaluationIndex
    val compiler = state.compiler
    val frame = state.frame

    // comments have been included below for documentation

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

    val res: KernelOutput = parsed map { validation =>
      val validationNel = validation.toValidationNel

      validationNel flatMap { statements =>
        val indexedWrapperName = Name(s"cmd$evaluationIndex")
        val wrapperName = Seq(Name("$sess"), indexedWrapperName)

        val munged: ValidationNel[LogError, MungedOutput] = Munger(
          compiler.parse,
          statements,
          s"$evaluationIndex",
          Seq(Name("$sess")),
          indexedWrapperName,
          frame.imports
        )

        val compiledAndLoaded: ValidationNel[LogError, (List[LogMessage], Evaluated)] = munged flatMap { processed =>
          val compilationResult =
            compiler.compile(processed.code.getBytes, processed.prefixCharLength, s"_ReplKernel$evaluationIndex.sc")

          compilationResult flatMap {
            case (logMessages, classFiles, imports) =>
              val loadedClass: Validation[LogError, Class[_]] = Validation.fromTryCatchNonFatal {
                for ((name, bytes) <- classFiles.sortBy(_._1)) {
                  frame.classloader.addClassFile(name, bytes)
                }
                Class.forName("$sess." + indexedWrapperName.backticked, true, frame.classloader)
              } leftMap (LogMessage.fromThrowable(_))

              val processed: Validation[LogError, Evaluated] = loadedClass flatMap { cls =>
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
                      if (filledPrefix.headOption.exists(_.backticked == "_root_"))
                        filledPrefix
                      else Seq(Name("_root_")) ++ filledPrefix

                    id.copy(prefix = rootedPrefix)
                  }
                )
                frame.addImports(newImports)

                evaluated map (Evaluated(wrapperName, newImports, _))
              }

              val mapped: Validation[LogError, SuccessfulInterpretation] = processed map (x => (logMessages, x))

              mapped.toValidationNel
          }
        }

        compiledAndLoaded map {
          case (logMessages, evaluated) => (logMessages, evaluated.value)
        }
      } leftMap( _.reverse)
    }

    // state mutation
    res match {
      case Some(Success(_)) => state = state.copy(evaluationIndex = state.evaluationIndex + 1)
      case _ => ()
    }

    res
  }

  def complete(text: String, position: Int) = {
    state.pressy.complete(text, position, Munger.importBlock(state.frame.imports))
  }

}

object ReplKernel {

  private case class KernelState(evaluationIndex: Int, frame: Frame, compiler: Compiler, pressy: Pressy)

  def apply(settings: Settings = new Settings()): ReplKernel = {
    val frame = {
      val currentClassLoader = Thread.currentThread().getContextClassLoader
      val hash = SpecialClassLoader.initialClasspathSignature(currentClassLoader)
      def special = new SpecialClassLoader(currentClassLoader, hash)
      new Frame(special, special, Imports(), Seq())
    }

    val dynamicClasspath = new VirtualDirectory("(memory)", None)

    val compiler = new Compiler(
      Classpath.classpath,
      dynamicClasspath,
      frame.classloader,
      frame.pluginClassloader,
      settings
    )

    val pressy = Pressy(
      Classpath.classpath,
      dynamicClasspath,
      frame.classloader,
      settings.copy()
    )

    new ReplKernel(KernelState(0, frame, compiler, pressy))
  }

}
