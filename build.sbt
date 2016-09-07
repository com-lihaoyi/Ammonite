fork := true

scalaVersion := "2.11.8"

crossScalaVersions := Seq(
  "2.11.3",
  "2.11.4",
  "2.11.5",
  "2.11.6",
  "2.11.7",
  "2.11.8"
)

organization := "com.lihaoyi"

version := _root_.ammonite.Constants.version

libraryDependencies ++= Seq("com.github.scopt" %% "scopt" % "3.4.0",
                            "com.lihaoyi" %% "upickle" % "0.4.2",
                            "com.lihaoyi" %% "pprint" % "0.4.2",
                            "org.apache.ivy" % "ivy" % "2.4.0",
                            "org.scalaj" %% "scalaj-http" % "2.3.0",
                            "org.scala-lang" % "scala-compiler" % scalaVersion.value,
                            "org.scala-lang" % "scala-reflect" % scalaVersion.value,
                            "com.lihaoyi" %% "scalaparse" % "0.3.7",
                            "com.lihaoyi" %% "ammonite-ops" % "0.7.6",
                            "com.chuusai" %% "shapeless" % "2.1.0" % "test",
                            "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
                            "org.scalatest" %% "scalatest" % "3.0.0" % "test",
                            "com.lihaoyi" %% "utest" % "0.4.3" % "test")

testFrameworks ++= Seq(new TestFramework("utest.runner.Framework"))

scalacOptions ++= Seq("-Ywarn-unused", "-Ywarn-unused-import", "-Ywarn-inaccessible", "-Ywarn-dead-code", "-Xlint")

autoCompilerPlugins := true

ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

(unmanagedSources in Compile) += (baseDirectory in ThisBuild).value / "project" / "Constants.scala"

mappings in (Compile, packageSrc) += {
  ((baseDirectory in ThisBuild).value / ".." / "project" / "Constants.scala") -> "Constants.scala"
}

javaOptions += "-Xmx4G"
