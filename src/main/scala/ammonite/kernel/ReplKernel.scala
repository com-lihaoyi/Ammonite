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
import ammonite.util.Name

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

    val res = parsed map { validation =>
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
          state.imports
        )

        /*val compiledAndLoaded: ValidationNel[LogError, (List[LogMessage], Imports, Any)] = */
        munged flatMap { processed =>
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
                      if (filledPrefix.headOption.exists(_.backticked == "_root_"))
                        filledPrefix
                      else Seq(Name("_root_")) ++ filledPrefix

                    id.copy(prefix = rootedPrefix)
                  }
                )
                evaluated map ((newImports, _))
              }

              val mapped = processed map (x => (logMessages, x._1, x._2))

              mapped.toValidationNel
          }
        }

      // compiledAndLoaded map {
      //   case (logMessages, _, value) => (logMessages, value)
      // }
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

  def complete(text: String, position: Int) = {
    state.pressy.complete(text, position, Munger.importBlock(state.imports))
  }

}

object ReplKernel {

  private case class KernelState(evaluationIndex: Int, frame: Frame, imports: Imports, compiler: Compiler, pressy: Pressy)

  def apply(settings: Settings = new Settings()): ReplKernel = {
    val currentClassLoader = Thread.currentThread().getContextClassLoader
    val hash = SpecialClassLoader.initialClasspathSignature(currentClassLoader)
    def special = new SpecialClassLoader(currentClassLoader, hash)

    val frame = new Frame(special, Seq())

    val dynamicClasspath = new VirtualDirectory("(memory)", None)

    val compiler = new Compiler(
      Classpath.classpath,
      dynamicClasspath,
      special,
      special,
      settings
    )

    val pressy = Pressy(
      Classpath.classpath,
      dynamicClasspath,
      special,
      settings.copy()
    )

    new ReplKernel(KernelState(0, frame, Imports(), compiler, pressy))
  }

}
