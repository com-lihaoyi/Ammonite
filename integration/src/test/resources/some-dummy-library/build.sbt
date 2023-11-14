val scalaV = sys.env("SCALA_VERSION")
val circeVersion = if(scalaV.startsWith("3.")) "0.14.6" else "0.12.0-M3"

lazy val root = (project in file(".")).
  settings(
    name := "some-dummy-library",
    organization := "com.lihaoyi",
    version := sys.env("VERSION"),
    scalaVersion := sys.env("SCALA_VERSION"),
    libraryDependencies += {
      if (sys.env("FIRST_RUN").toBoolean)
        "io.circe" %% "circe-core" % circeVersion
      else
        "io.argonaut" %% "argonaut" % "6.2.6"
    }
  )
