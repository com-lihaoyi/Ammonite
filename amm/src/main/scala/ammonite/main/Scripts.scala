package ammonite.main
import java.nio.file.NoSuchFileException
import java.util.function.Supplier

import ammonite.compiler.{CodeClassWrapper, ObjectCodeWrapper}
import ammonite.compiler.iface.CodeSource
import ammonite.interp.api.AmmoniteExit
import ammonite.util.{Name, Res, Util}
import fastparse.internal.Util.literalize

/**
  * Logic around using Ammonite as a script-runner; invoking scripts via the
  * macro-generated [[Router]], and pretty-printing any output or error messages
  */
object Scripts {
  def runScript(wd: os.Path,
                path: os.Path,
                interp: ammonite.interp.Interpreter,
                scriptArgs: Seq[String] = Nil) = {
    interp.watch(path)
    val (pkg, wrapper) = Util.pathToPackageWrapper(Seq(), path relativeTo wd)

    for{
      scriptTxt <- try Res.Success(Util.normalizeNewlines(os.read(path))) catch{
        case e: NoSuchFileException => Res.Failure("Script file not found: " + path)
      }

      parent = s"_root_.java.util.function.Supplier[mainargs.ParserForMethods[$$routesOuter.type]]"
      processed <- interp.processModule(
        scriptTxt,
        new CodeSource(wrapper.raw, pkg.map(_.raw).toArray, Array("ammonite", "$file"), path.toNIO),
        autoImport = true,
        // Not sure why we need to wrap this in a separate `$routes` object,
        // but if we don't do it for some reason the `generateRoutes` macro
        // does not see the annotations on the methods of the outer-wrapper.
        // It can inspect the type and its methods fine, it's just the
        // `methodsymbol.annotations` ends up being empty.
        extraCode = Util.normalizeNewlines(
          s"""
          |val $$routesOuter = this
          |object $$routes
          |extends $parent{
          |  def get() = mainargs.ParserForMethods[$$routesOuter.type]($$routesOuter)
          |}
          """.stripMargin
        ),
        hardcoded = true
      )

      routeClsName <- processed.blockInfo.lastOption match{
        case Some(meta) => Res.Success(meta.id.wrapperPath)
        case None => Res.Skip
      }

      scriptMains = interp.scriptCodeWrapper match{
        case ObjectCodeWrapper =>
          Some(
            interp
              .evalClassloader
              .loadClass(routeClsName + "$$routes$")
              .getField("MODULE$")
              .get(null)
              .asInstanceOf[Supplier[Object]] // Supplier[mainargs.ParserForMethods[Any]]
              .get()
          )

        case CodeClassWrapper =>
          val outer = interp
            .evalClassloader
            .loadClass(routeClsName)
            .getMethod("instance")
            .invoke(null)

          Some(
            outer.getClass.getMethod("$routes").invoke(outer)
              .asInstanceOf[Supplier[Object]] // mainargs.ParserForMethods[Any]
              .get()
          )
        case _ => None
      }

      res <- Util.withContextClassloader(interp.evalClassloader){
        scriptMains match {
          case Some(parser) =>
            val scriptInvoker: ammonite.interp.api.ScriptInvoker = {
              val cls = interp.evalClassloader.loadClass("ammonite.interp.api.ScriptInvokerImpl")
              cls.getConstructor().newInstance().asInstanceOf[ammonite.interp.api.ScriptInvoker]
            }
            if (scriptArgs.take(1) == Seq("--help")){
              Res.Success(
                new Object{
                  override def toString() = scriptInvoker.helpText(
                    parser,
                    100,
                    false
                  )
                }
              )
            }else scriptInvoker.invoke(
              parser,
              path.last,
              scriptArgs.toArray
            ) match {
              case s: ammonite.interp.api.ScriptInvoker.Result.Success[_] =>
                Res.Success(s.value)
              case f: ammonite.interp.api.ScriptInvoker.Result.Failure =>
                Res.Failure(f.message)
              case e: ammonite.interp.api.ScriptInvoker.Result.ExceptionThrown =>
                e.exception match {
                  case x: AmmoniteExit => Res.Success(x.value)
                  case e => Res.Exception(e, "")
                }
            }
        }
      }
    } yield res
  }
}
