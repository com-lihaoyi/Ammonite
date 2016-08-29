lazy val root = (project in file(".")).
  settings(
    name := "some-dummy-library",
    organization := "com.lihaoyi",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.8"
  )
