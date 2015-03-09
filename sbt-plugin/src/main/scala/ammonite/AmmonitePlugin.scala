package ammonite

import sbt._, Keys._

object AmmonitePlugin extends AutoPlugin {

  // Inspired by https://github.com/alexarchambault/sbt-notebook

  lazy val Ammonite = config("ammonite") extend (Compile, Runtime)

  object autoImport {
    val ammoniteVersion = settingKey[String]("Ammonite version")
    val repl = taskKey[Unit]("Run an Ammonite REPL")
  }

  import autoImport._

  override def trigger = allRequirements

  override lazy val projectSettings = inConfig(Ammonite)(
    Defaults.compileSettings ++ Defaults.runnerSettings ++ Classpaths.ivyBaseSettings ++
    Seq(
      /* Overriding run and runMain defined by compileSettings so that they use the scoped fullClasspath */
      run <<= Defaults.runTask(fullClasspath, mainClass in run, runner in run),
      runMain <<= Defaults.runMainTask(fullClasspath, runner in run),
      /* Overriding classDirectory defined by compileSettings so that we are given
        the classDirectory of the default scope in runMain below */
      classDirectory := crossTarget.value / "classes",
      /* Adding ammonite-repl dependency */
      libraryDependencies += "com.lihaoyi" %% "ammonite-repl" % (ammoniteVersion in Runtime).value,
      connectInput := true
    )
  ) ++ Seq(
    ammoniteVersion := "0.2.4",
    repl <<= Def.taskDyn {
      /* Compiling the root project, so that its build products and those of its dependency sub-projects are available
         in the classpath */
      (compile in Runtime).value

      val mainClass = "ammonite.repl.Repl"
      (runMain in Ammonite).toTask(s" $mainClass")
    }
  )

}
