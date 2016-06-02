import scalatex.ScalatexReadme
import sbtassembly.AssemblyPlugin.defaultShellScript



scalaVersion := "2.11.6"

crossScalaVersions := Seq(
  "2.10.3", "2.10.4", "2.10.5", "2.10.6",
  "2.11.3", "2.11.4", "2.11.5", "2.11.6", "2.11.7", "2.11.8"
)

val dontPublishSettings = Seq(
  publishArtifact := false,
  publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
)

dontPublishSettings

val macroSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
  ) ++ (
    if (scalaVersion.value startsWith "2.11.") Nil
    else Seq(
      compilerPlugin("org.scalamacros" % s"paradise" % "2.0.1" cross CrossVersion.full),
      "org.scalamacros" %% s"quasiquotes" % "2.0.1"
    )
  )
)

val sharedSettings = Seq(
  scalaVersion := "2.11.8",
  organization := "com.lihaoyi",
  version := _root_.ammonite.Constants.version,
  libraryDependencies += "com.lihaoyi" %% "utest" % "0.4.3" % "test",
  // Needed for acyclic to work...
  libraryDependencies ++= {
    if (scalaVersion.value startsWith "2.11.") Nil
    else Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
    )
  },
  testFrameworks := Seq(new TestFramework("utest.runner.Framework")),
  scalacOptions += "-target:jvm-1.7",
  autoCompilerPlugins := true,
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.4"),
  ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) },
  parallelExecution in Test := !scalaVersion.value.contains("2.10"),
  (unmanagedSources in Compile) += baseDirectory.value/".."/"project"/"Constants.scala",
  mappings in (Compile, packageSrc) += {
    (baseDirectory.value/".."/"project"/"Constants.scala") -> "Constants.scala"
  },
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "acyclic" % "0.1.4" % "provided"
  ) ,
  publishTo := Some(
    "releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  ),
  pomExtra :=
    <url>https://github.com/lihaoyi/Ammonite</url>
      <licenses>
        <license>
          <name>MIT license</name>
          <url>http://www.opensource.org/licenses/mit-license.php</url>
        </license>
      </licenses>
      <scm>
        <url>git://github.com/lihaoyi/Ammonite.git</url>
        <connection>scm:git://github.com/lihaoyi/Ammonite.git</connection>
      </scm>
      <developers>
        <developer>
          <id>lihaoyi</id>
          <name>Li Haoyi</name>
          <url>https://github.com/lihaoyi</url>
        </developer>
      </developers>
) ++ prompt

/**
 * Concise, type-safe operating-system operations in Scala: filesystem,
 * subprocesses, and other such things.
 */
lazy val ops = project
  .settings(
    sharedSettings,
    name := "ammonite-ops"
  )


lazy val prompt = shellPrompt in ThisBuild := { state =>
  val name = Project.extract(state).currentRef.project
  (if (name == "ammonite") "" else name) + "> "
}

/**
 * A standalone re-implementation of a composable readline-style REPL,
 * without any behavior associated with it. Contains an echo-repl that
 * can be run to test the REPL interactions
 */
lazy val terminal = project
  .settings(
    sharedSettings,
    name := "ammonite-terminal",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "sourcecode" % "0.1.1",
      "com.lihaoyi" %% "fansi" % "0.1.1"
    ),
    macroSettings
  )


/**
 * A better Scala REPL, which can be dropped in into any project or run
 * standalone in a Scala project to provide a better interactive experience
 * for Scala
 */
lazy val repl = project
  .dependsOn(terminal, ops)
  .settings(
    macroSettings,
    sharedSettings,
    crossVersion := CrossVersion.full,
    test in assembly := {},
    name := "ammonite-repl",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "jline" % "jline" % "2.12",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.apache.ivy" % "ivy" % "2.4.0",
      "com.lihaoyi" %% "scalaparse" % "0.3.7",
      "com.lihaoyi" %% "upickle" % "0.4.0",
      "com.lihaoyi" %% "pprint" % "0.4.0",
      "com.github.scopt" %% "scopt" % "3.4.0"
    ),
    libraryDependencies ++= (
      if (scalaVersion.value startsWith "2.10.") Nil
      else Seq("com.chuusai" %% "shapeless" % "2.1.0" % "test")
    ),
    javaOptions += "-Xmx4G",
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(
      prependShellScript = Some(
        // G1 Garbage Collector is awesome https://github.com/lihaoyi/Ammonite/issues/216
        Seq("#!/usr/bin/env sh", """exec java -jar -XX:+UseG1GC "$0" "$@"""")
      )
    ),
    assemblyJarName in assembly := s"${name.value}-${version.value}-${scalaVersion.value}"
  )

/**
 * Project that binds together [[ops]] and [[repl]], turning them into a
 * credible systems shell that can be used to replace bash/zsh/etc. for
 * common housekeeping tasks
 */
lazy val shell = project
  .dependsOn(ops, repl % "compile->compile;test->test")
  .settings(
    sharedSettings,
    macroSettings,
    name := "ammonite-shell",
    libraryDependencies ++= {
      if (!scalaVersion.value.startsWith("2.11.")) Nil
      else Seq("com.geirsson" %% "scalafmt" % "0.2.5")
    },
    (test in Test) <<= (test in Test).dependsOn(packageBin in Compile),
    (run in Test) <<= (run in Test).dependsOn(packageBin in Compile),
    (testOnly in Test) <<= (testOnly in Test).dependsOn(packageBin in Compile)
  )

val integrationTasks = Seq(
  assembly in repl,
  packageBin in (shell, Compile)
)
lazy val integration = project
  .dependsOn(ops)
  .dependsOn(repl)
  .settings(
    sharedSettings,
    (test in Test) <<= (test in Test).dependsOn(integrationTasks:_*),
    (run in Test) <<= (run in Test).dependsOn(integrationTasks:_*),
    (testOnly in Test) <<= (testOnly in Test).dependsOn(integrationTasks:_*),
    (console in Test) <<= (console in Test).dependsOn(integrationTasks:_*),
    dontPublishSettings,
    initialCommands in (Test, console) := "ammonite.integration.Main.main(null)"
  )


/**
 * REPL available via remote ssh access.
 * Plug into any app environment for live hacking on a live application.
 */
lazy val sshd = project
    .dependsOn(repl)
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

lazy val readme = ScalatexReadme(
  projectId = "readme",
  wd = file(""),
  url = "https://github.com/lihaoyi/ammonite/tree/master",
  source = "Index"
).settings(
  dontPublishSettings,
  scalaVersion := "2.11.8",
  (run in Compile) <<= (run in Compile).dependsOn(
    assembly in repl,
    packageBin in (shell, Compile),
    doc in (ops, Compile),
    doc in (terminal, Compile),
    doc in (repl, Compile),
    doc in (sshd, Compile),
    doc in (shell, Compile)
  ),
  (run in Compile) <<= (run in Compile).dependsOn(Def.task{
    val apiFolder = (target in Compile).value/"scalatex"/"api"
    val copies = Seq(
      (doc in (ops, Compile)).value -> "ops",
      (doc in (terminal, Compile)).value -> "terminal",
      (doc in (repl, Compile)).value -> "repl",
      (doc in (sshd, Compile)).value -> "sshd",
      (doc in (shell, Compile)).value -> "shell"
    )
    for ((folder, name) <- copies){
      sbt.IO.copyDirectory(folder, apiFolder/name, overwrite = true)
    }
  }),
  (unmanagedSources in Compile) += baseDirectory.value/".."/"project"/"Constants.scala"
)

lazy val tested = project
  .in(file("target/tested"))
  .aggregate(published, integration)
  .settings(dontPublishSettings)

lazy val published = project
  .in(file("target/published"))
  .aggregate(ops, shell, terminal, repl, sshd)
  .settings(dontPublishSettings)
