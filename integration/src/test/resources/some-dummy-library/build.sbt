
lazy val root = (project in file(".")).
  settings(
    name := "some-dummy-library",
    organization := "com.lihaoyi",
    version := sys.env("VERSION"),
    scalaVersion := sys.env("SCALA_VERSION"),
    libraryDependencies += {
      if (java.lang.Boolean.parseBoolean(sys.env("FIRST_RUN")))
        "io.circe" %% "circe-core" % "0.10.0"
      else
        "io.argonaut" %% "argonaut" % "6.2.2"
    }
  )
