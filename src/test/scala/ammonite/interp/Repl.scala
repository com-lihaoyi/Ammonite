package ammonite.interp

import jline.console.CursorBuffer

import scala.tools.nsc.Settings

object Repl {
  def main(args: Array[String]): Unit = {
    import scala.tools.nsc.interpreter._
    val settings = new Settings
    settings.embeddedDefaults[Repl.type]

    val loop = new ILoop(){

      override def prompt = "ammonite>"
      override def createInterpreter() {
        if (addedClasspath != "")
          settings.classpath append addedClasspath
        println("MY SPECIAL INTERPRETER SNOWFLAKE")
        intp = new IMainX
      }


      var stashed: CursorBuffer = null
    }


    loop.process(settings)

  }
}
