import scalatex.ScalatexReadme

lazy val readme = ScalatexReadme(
  projectId = "readme",
  wd = file(""),
  url = "https://github.com/lihaoyi/ammonite/tree/master",
  source = "Index"
).settings(
  dontPublishSettings,
  scalaVersion := "2.12.3",
  libraryDependencies += "com.lihaoyi" %% "fansi" % "0.2.3",
  (run in Compile) := (run in Compile).dependsOn(
    assembly in (amm, Test),
    packageBin in (shell, Compile),
    doc in (ops, Compile),
    doc in (terminal, Compile),
    doc in (amm, Compile),
    doc in (sshd, Compile),
    doc in (shell, Compile)
  ).evaluated,
  (run in Compile) := (run in Compile).dependsOn(Def.task{
    val apiFolder = (target in Compile).value/"scalatex"/"api"
    val copies = Seq(
      (doc in (ops, Compile)).value -> "ops",
      (doc in (terminal, Compile)).value -> "terminal",
      (doc in (amm, Compile)).value -> "amm",
      (doc in (sshd, Compile)).value -> "sshd",
      (doc in (shell, Compile)).value -> "shell"
    )
    for ((folder, name) <- copies){
      sbt.IO.copyDirectory(folder, apiFolder/name, overwrite = true)
    }
  }).evaluated,
  envVars in Test := Map(
    "AMMONITE_TEST_SHELL" -> (packageBin in Compile).value.toString,
    "AMMONITE_TEST_ASSEMBLY" -> (assembly in amm).value.toString
  ),
  envVars in run := Map(
    "AMMONITE_TEST_SHELL" -> (packageBin in (shell, Compile)).value.toString
  ),
  fork := true,
  baseDirectory in (Compile, run) := (baseDirectory in (Compile, run)).value / "..",
  (unmanagedSources in Compile) += baseDirectory.value/".."/"project"/"Constants.scala"
)
