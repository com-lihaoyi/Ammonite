
organization := "com.lihaoyi"

scalaVersion := "2.11.4"

crossScalaVersions := Seq("2.10.4", "2.11.4")

version := "0.1.0"

name := "ammonite"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.2.4"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value % "test",
  "jline" % "jline" % "2.12" % "test"
)

testFrameworks += new TestFramework("utest.runner.JvmFramework")

initialCommands in console := """
  import ammonite._
  var wd = processWorkingDir
"""


