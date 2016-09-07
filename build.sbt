import scalatex.ScalatexReadme
import sbtassembly.AssemblyPlugin.defaultShellScript

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

val dontPublishSettings = Seq(
  publishArtifact := false,
  publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
)

dontPublishSettings

val sharedSettings = Seq(
  scalaVersion := "2.11.8",
  organization := "com.lihaoyi",
  version := _root_.ammonite.Constants.version,
  libraryDependencies ++= Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
                              "org.scalatest" %% "scalatest" % "3.0.0" % "test",
                              "com.lihaoyi" %% "utest" % "0.4.3" % "test"),
  testFrameworks ++= Seq(new TestFramework("utest.runner.Framework")),
  scalacOptions ++= Seq("-target:jvm-1.7",
                        "-Ywarn-unused",
                        "-Ywarn-unused-import",
                        "-Ywarn-inaccessible",
                        "-Ywarn-dead-code",
                        "-Xlint"),
  autoCompilerPlugins := true,
  ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) },
  (unmanagedSources in Compile) += (baseDirectory in ThisBuild).value / "project" / "Constants.scala",
  mappings in (Compile, packageSrc) += {
    ((baseDirectory in ThisBuild).value / ".." / "project" / "Constants.scala") -> "Constants.scala"
  }
)

/**
  * Concise, type-safe operating-system operations in Scala: filesystem,
  * subprocesses, and other such things.
  */
lazy val ops = project.settings(
  sharedSettings,
  name := "ammonite-ops"
)

/**
  * A better Scala REPL, which can be dropped in into any project or run
  * standalone in a Scala project to provide a better interactive experience
  * for Scala
  */
lazy val amm = project
  .dependsOn(ops)
  .settings(
    sharedSettings,
    crossVersion := CrossVersion.full,
    test in assembly := {},
    name := "ammonite",
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "3.4.0",
      "com.lihaoyi" %% "upickle" % "0.4.2",
      "com.lihaoyi" %% "pprint" % "0.4.2",
      "org.apache.ivy" % "ivy" % "2.4.0",
      "org.scalaj" %% "scalaj-http" % "2.3.0",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.lihaoyi" %% "scalaparse" % "0.3.7"
    ),
    libraryDependencies ++= (
      if (scalaVersion.value startsWith "2.10.") Nil
      else Seq("com.chuusai" %% "shapeless" % "2.1.0" % "test")
    ),
    fork := true,
    javaOptions += "-Xmx4G",
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(
      prependShellScript = Some(
        // G1 Garbage Collector is awesome https://github.com/lihaoyi/Ammonite/issues/216
        Seq("#!/usr/bin/env sh", """exec java -jar -Xmx500m -XX:+UseG1GC $JAVA_OPTS "$0" "$@"""")
      )
    ),
    assemblyJarName in assembly := s"${name.value}-${version.value}-${scalaVersion.value}",
    assembly in Test := {
      val dest = assembly.value.getParentFile / "amm"
      IO.copyFile(assembly.value, dest)
      import sys.process._
      Seq("chmod", "+x", dest.getAbsolutePath).!
      dest
    }
  )
