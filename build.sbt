import scalatex.ScalatexReadme

lazy val readme = ScalatexReadme(
  projectId = "readme",
  wd = file(""),
  url = "https://github.com/com-lihaoyi/Ammonite/tree/master",
  source = "Index"
).settings(
  scalaVersion := "2.13.5",
  libraryDependencies += "com.lihaoyi" %% "fansi" % "0.2.12",
  libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.7.3",
  envVars in Test := Map(
    "AMMONITE_ASSEMBLY" -> sys.env("AMMONITE_ASSEMBLY"),
    "AMMONITE_SHELL" -> sys.env("AMMONITE_SHELL")
  ),
  fork := true,
  baseDirectory in (Compile, run) := (baseDirectory in (Compile, run)).value / "..",
  (unmanagedSources in Compile) += file(sys.env("CONSTANTS_FILE"))
)
