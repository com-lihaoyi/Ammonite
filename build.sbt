import scalatex.ScalatexReadme
import sbtassembly.AssemblyPlugin.defaultShellScript

fork := true

scalaVersion := "2.11.8"

crossScalaVersions := Seq(
  "2.10.4",
  "2.10.5",
  "2.10.6",
  "2.11.3",
  "2.11.4",
  "2.11.5",
  "2.11.6",
  "2.11.7",
  "2.11.8"
)

val dontPublishSettings = Seq(
  publishArtifact := false,
  publishTo := Some(
    Resolver.file("Unused transient repository", file("target/unusedrepo")))
)

dontPublishSettings

val sharedSettings = Seq(
  scalaVersion := "2.11.8",
  organization := "com.lihaoyi",
  version := _root_.ammonite.Constants.version,
  libraryDependencies ++= Seq(
     "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
     "org.scalatest" %% "scalatest" % "3.0.0" % "test",
    "com.lihaoyi" %% "utest" % "0.4.3" % "test"),
  scalafmtConfig := Some(file(".scalafmt")),
  testFrameworks ++= Seq(new TestFramework("utest.runner.Framework")),
  scalacOptions ++= Seq("-target:jvm-1.7", "-Ywarn-unused", "-Ywarn-unused-import", "-Ywarn-inaccessible", "-Ywarn-dead-code", "-Xlint"),
  autoCompilerPlugins := true,
  ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) },
  parallelExecution in Test := !scalaVersion.value.contains("2.10"),
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
  .dependsOn(
    ops,
    ammUtil,
    ammRuntime,
    ammInterp,
    ammRepl
  )
  .settings(
    sharedSettings,
    crossVersion := CrossVersion.full,
    test in assembly := {},
    name := "ammonite",
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "3.4.0"
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
        Seq("#!/usr/bin/env sh",
            """exec java -jar -Xmx500m -XX:+UseG1GC $JAVA_OPTS "$0" "$@"""")
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

lazy val ammUtil = project
  .in(file("amm/util"))
  .dependsOn(ops)
  .settings(
    sharedSettings,
    crossVersion := CrossVersion.full,
    name := "ammonite-util",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "0.4.2",
      "com.lihaoyi" %% "pprint" % "0.4.2"
    )
  )

lazy val ammRuntime = project
  .in(file("amm/runtime"))
  .dependsOn(ops, ammUtil)
  .settings(
    sharedSettings,
    crossVersion := CrossVersion.full,
    name := "ammonite-runtime",
    libraryDependencies ++= Seq(
      "org.apache.ivy" % "ivy" % "2.4.0",
      "org.scalaj" %% "scalaj-http" % "2.3.0"
    )
  )

lazy val ammInterp = project
  .in(file("amm/interp"))
  .dependsOn(ops, ammUtil, ammRuntime)
  .settings(
    sharedSettings,
    crossVersion := CrossVersion.full,
    name := "ammonite-compiler",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.lihaoyi" %% "scalaparse" % "0.3.7"
    )
  )

lazy val ammRepl = project
  .in(file("amm/repl"))
  .dependsOn(
    ammUtil, 
    ammRuntime, 
    ammInterp)
  .settings(
    sharedSettings,
    crossVersion := CrossVersion.full,
    name := "ammonite-repl",
    libraryDependencies ++= Seq(
      "jline" % "jline" % "2.12"
    )
  )

/**
  * Project that binds together [[ops]] and [[amm]], turning them into a
  * credible systems shell that can be used to replace bash/zsh/etc. for
  * common housekeeping tasks
  */
lazy val shell = project
  .dependsOn(ops, amm % "compile->compile;test->test")
  .settings(
    sharedSettings,
    name := "ammonite-shell",
    (test in Test) <<= (test in Test).dependsOn(packageBin in Compile),
    (run in Test) <<= (run in Test).dependsOn(packageBin in Compile),
    (testOnly in Test) <<= (testOnly in Test).dependsOn(packageBin in Compile)
  )

val integrationTasks = Seq(
  assembly in amm,
  packageBin in (shell, Compile)
)
lazy val integration = project
  .dependsOn(ops)
  .dependsOn(amm)
  .settings(
    sharedSettings,
    (test in Test) <<= (test in Test).dependsOn(integrationTasks: _*),
    (run in Test) <<= (run in Test).dependsOn(integrationTasks: _*),
    (testOnly in Test) <<= (testOnly in Test).dependsOn(integrationTasks: _*),
    (console in Test) <<= (console in Test).dependsOn(integrationTasks: _*),
    parallelExecution in Test := false,
    dontPublishSettings,
    initialCommands in (Test, console) := "ammonite.integration.Main.main(null)"
  )

/**
  * REPL available via remote ssh access.
  * Plug into any app environment for live hacking on a live application.
  */
lazy val sshd = project
  .dependsOn(amm)
  .settings(
    sharedSettings,
    crossVersion := CrossVersion.full,
    name := "ammonite-sshd",
    libraryDependencies ++= Seq(
      "org.apache.sshd" % "sshd-core" % "0.14.0",
      //-- test --//
      // slf4j-nop makes sshd server use logger that writes into the void
      "org.slf4j" % "slf4j-nop" % "1.7.12" % "test",
      "com.jcraft" % "jsch" % "0.1.53" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"
    )
  )

lazy val published = project
  .in(file("target/published"))
  .aggregate(ops,
             shell,
             amm,
             sshd,
             ammUtil,
             ammRuntime,
             ammInterp,
             ammRepl)
  .settings(dontPublishSettings)
