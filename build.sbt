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
                            "ch.qos.logback" % "logback-classic" % "1.1.7",
                            "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0",
                            "org.scalatest" %% "scalatest" % "3.0.0" % "test")

scalacOptions ++= Seq("-Ywarn-unused", "-Ywarn-unused-import", "-Ywarn-inaccessible", "-Ywarn-dead-code", "-Xlint")

autoCompilerPlugins := true

ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

(unmanagedSources in Compile) += (baseDirectory in ThisBuild).value / "project" / "Constants.scala"

mappings in (Compile, packageSrc) += {
  ((baseDirectory in ThisBuild).value / ".." / "project" / "Constants.scala") -> "Constants.scala"
}

javaOptions += "-Xmx4G"

logBuffered in Test := false

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oD")
