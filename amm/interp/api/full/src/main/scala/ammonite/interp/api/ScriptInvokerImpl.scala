package ammonite.interp.api

class ScriptInvokerImpl extends ScriptInvoker {

  def invoke(
    parser: Object,
    scriptName: String,
    scriptArgs: Array[String]
  ): ScriptInvoker.Result =
    invoke(
      parser.asInstanceOf[mainargs.ParserForMethods[Any]].mains,
      scriptName,
      scriptArgs
    )

  def helpText(parser: Object, totalWidth: Int, docsOnNewLine: Boolean): String =
    parser
      .asInstanceOf[mainargs.ParserForMethods[Any]]
      .helpText(
        totalWidth = 100,
        docsOnNewLine = false
      )

  def invoke[B](
    mains: mainargs.MethodMains[B],
    scriptName: String,
    scriptArgs: Array[String]
  ): ScriptInvoker.Result =
    if (mains.value.isEmpty) {
      // If there are no @main methods, there's nothing to do
      if (scriptArgs.isEmpty) new ScriptInvoker.Result.Success(().asInstanceOf[Object])
      else new ScriptInvoker.Result.Failure(
        "Script " + scriptName +
        " does not take arguments: " + scriptArgs.map(pprint.Util.literalize(_)).mkString(" ")
      )
    } else
      // If there's one @main method, we run it with all args
      mainargs.Invoker.runMains(
        mains,
        scriptArgs,
        allowPositional = true,
        allowRepeats = false
      ) match {
        case Left(earlyError) =>
          new ScriptInvoker.Result.Failure(mainargs.Renderer.renderEarlyError(earlyError))
        case Right((mainData, result)) =>
          result match{
            case mainargs.Result.Success(x) => new ScriptInvoker.Result.Success(x)
            case mainargs.Result.Failure.Exception(x) =>
              new ScriptInvoker.Result.ExceptionThrown(x)
            case res: mainargs.Result.Failure =>
              new ScriptInvoker.Result.Failure(
                mainargs.Renderer.renderResult(
                  mainData,
                  res,
                  totalWidth = 100,
                  printHelpOnError = true,
                  docsOnNewLine = false,
                  customName = None,
                  customDoc = None
                )
              )
          }
      }

}

object ScriptInvokerImpl extends ScriptInvokerImpl
