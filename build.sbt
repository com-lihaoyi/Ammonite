import scalatex.ScalatexReadme

lazy val readme = ScalatexReadme(
  projectId = "readme",
  wd = file(""),
  url = "https://github.com/com-lihaoyi/Ammonite/tree/master",
  source = "Index"
).settings(
  scalaVersion := "2.12.18",
  libraryDependencies += "com.lihaoyi" %% "fansi" % "0.2.3",
  libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.7.8",
  Test / envVars := Map(
    "AMMONITE_ASSEMBLY" -> sys.env("AMMONITE_ASSEMBLY"),
    "AMMONITE_SHELL" -> sys.env("AMMONITE_SHELL")
  ),
  fork := true,
  Compile / run / baseDirectory := (Compile / run / baseDirectory).value / "..",
  (Compile / unmanagedSources) += file(sys.env("CONSTANTS_FILE"))
)
