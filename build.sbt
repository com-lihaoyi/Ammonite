
organization := "com.lihaoyi"

scalaVersion := "2.11.4"

name := "ammonite"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.2.4"

testFrameworks += new TestFramework("utest.runner.JvmFramework")

initialCommands in console := """
  import ammonite._
  var wd = cwd
"""