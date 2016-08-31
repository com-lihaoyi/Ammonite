
scalaVersion := "2.11.8"
crossScalaVersions := Seq("2.10.6", "2.11.8")

lazy val root = (project in file(".")).
  settings(
    name := "some-dummy-library",
    organization := "com.lihaoyi",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.8"
  )
