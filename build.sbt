import scalatex.ScalatexReadme

scalaVersion := "2.12.4"

crossScalaVersions := Seq(
  "2.11.3", "2.11.4", "2.11.5", "2.11.6", "2.11.7", "2.11.8", "2.11.9"
)

val dontPublishSettings = Seq(
  publishArtifact := false,
  publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
)

dontPublishSettings

val macroSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
  )
)

val sharedSettings = Seq(

  scalaVersion := "2.12.4",
  organization := "com.lihaoyi",
  version := _root_.ammonite.Constants.version,
  libraryDependencies += "com.lihaoyi" %% "utest" % "0.6.0" % Test,
  // Needed for acyclic to work...
  testFrameworks := Seq(new TestFramework("utest.runner.Framework")),
  scalacOptions += "-target:jvm-1.7",
  scalacOptions += "-P:acyclic:force",
  autoCompilerPlugins := true,
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.7"),
  ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) },
  parallelExecution in Test := false,
  resolvers += Resolver.sonatypeRepo("releases"),
  (unmanagedSources in Compile) += (baseDirectory in ThisBuild).value/"project"/"Constants.scala",
  mappings in (Compile, packageSrc) += {
    ((baseDirectory in ThisBuild).value/".."/"project"/"Constants.scala") -> "Constants.scala"
  },
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "acyclic" % "0.1.7" % Provided
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
)

/**
 * Concise, type-safe operating-system operations in Scala: filesystem,
 * subprocesses, and other such things.
 */
lazy val ops = project
  .settings(
    sharedSettings,
    libraryDependencies += "com.lihaoyi" %% "geny" % "0.1.2",
    name := "ammonite-ops"
  )


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
      "com.lihaoyi" %% "sourcecode" % "0.1.3",
      "com.lihaoyi" %% "fansi" % "0.2.3"
    ),
    macroSettings
  )


/**
 * A better Scala REPL, which can be dropped in into any project or run
 * standalone in a Scala project to provide a better interactive experience
 * for Scala
 */

lazy val amm = project
  .dependsOn(
    terminal, ops,
    ammUtil, ammRuntime, ammInterp, ammRepl % "compile->compile;test->test"
  )
  .settings(
    macroSettings,
    sharedSettings,

    unmanagedSourceDirectories in Compile ++= {
      if (Set("2.12", "2.11").contains(scalaBinaryVersion.value))
        Seq(baseDirectory.value / "src" / "main" / "scala-2.11_2.12")
      else
        Seq()
    },

    // Aggregate source jars into the assembly classpath, so that the
    // `source` macro can find their sources and highlight/display them.
    (fullClasspath in Runtime) ++= {
      (updateClassifiers in Runtime).value
        .configurations
        .find(_.configuration == Runtime.name)
        .get
        .modules
        .flatMap(_.artifacts)
        .collect{case (a, f) if a.classifier == Some("sources") => f}
    },

    // This adds Ammonite's *own* source jars to the `amm/test:assembly`.
    // We could, but don't, do it for `amm/test:run` because it's slow and
    // annoying to run every time I just want to open a test REPL
    fullClasspath in assembly ++= Seq(
      (packageSrc in (ops, Compile)).value,
      (packageSrc in (terminal, Compile)).value,
      (packageSrc in (ammUtil, Compile)).value,
      (packageSrc in (ammRuntime, Compile)).value,
      (packageSrc in (ammInterp, Compile)).value,
      (packageSrc in (ammRepl, Compile)).value,
      (packageSrc in Compile).value
    ),

    crossVersion := CrossVersion.full,
    test in assembly := {},
    name := "ammonite",
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "3.5.0"
    ),
    libraryDependencies ++= Seq("com.chuusai" %% "shapeless" % "2.3.2" % Test),
    javaOptions += "-Xmx4G",


    assemblyOption in assembly := (assemblyOption in assembly).value.copy(
      prependShellScript = {
        def universalScript(shellCommands: Seq[String],
                            cmdCommands: Seq[String],
                            shebang: Boolean = true): Seq[String] = {
          Seq(
            Seq("#!/usr/bin/env sh")
              .filter(_ => shebang),
            Seq("shopt -s expand_aliases", "alias ::=''")
              .map(line => s":; $line"),
            (shellCommands :+ "exit")
              .map(line => s":: $line"),
            "@echo off" +: cmdCommands :+ "exit /B",
            Seq("\r\n")
          ).flatten
        }

        def defaultUniversalScript(javaOpts: Seq[String] = Seq.empty, shebang: Boolean = true): Seq[String] = {
          val javaOptsString = javaOpts.map(_ + " ").mkString
          universalScript(
            shellCommands = Seq(s"exec java -jar $javaOptsString" + """$JAVA_OPTS "$0" "$@""""),
            cmdCommands = Seq(s"java -jar $javaOptsString" + """%JAVA_OPTS% "%~dpnx0" %*"""),
            shebang = shebang
          )
        }

        // G1 Garbage Collector is awesome https://github.com/lihaoyi/Ammonite/issues/216
        Some(defaultUniversalScript(Seq("-Xmx500m", "-XX:+UseG1GC")))
      }
    ),
    assemblyJarName in assembly := s"${name.value}-${version.value}-${scalaVersion.value}",
    assembly in Test := {
      val dest = target.value/"amm"
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
    macroSettings,
    sharedSettings,
    name := "ammonite-util",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "0.6.2",
      "com.lihaoyi" %% "pprint" % "0.5.2",
      "com.lihaoyi" %% "fansi" % "0.2.4"
    )
  )

lazy val ammRuntime = project
  .in(file("amm/runtime"))
  .dependsOn(ops, ammUtil)
  .settings(
    macroSettings,
    sharedSettings,

    name := "ammonite-runtime",
    libraryDependencies ++= Seq(
      "io.get-coursier" %% "coursier" % "1.0.0",
      "io.get-coursier" %% "coursier-cache" % "1.0.0",
      "org.scalaj" %% "scalaj-http" % "2.3.0"
    )
  )


lazy val ammInterp = project
  .in(file("amm/interp"))
  .dependsOn(ops, ammUtil, ammRuntime)
  .settings(
    macroSettings,
    sharedSettings,
    crossVersion := CrossVersion.full,

    name := "ammonite-compiler",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "com.lihaoyi" %% "scalaparse" % "1.0.0",
      "org.javassist" % "javassist" % "3.21.0-GA"

    ),
    unmanagedSourceDirectories in Compile ++= {
      if (Set("2.11").contains(scalaBinaryVersion.value))
        Seq(baseDirectory.value / "src" / "main" / "scala-2.10_2.11")
      else
        Seq()
    }
  )


lazy val ammRepl = project
  .in(file("amm/repl"))
  .dependsOn(terminal, ammUtil, ammRuntime, ammInterp)
  .settings(
    macroSettings,
    sharedSettings,
    crossVersion := CrossVersion.full,
    name := "ammonite-repl",
    libraryDependencies ++= Seq(
      "org.jline" % "jline" % "3.6.2",
      "org.jline" % "jline-terminal-jna" % "3.6.2",
      "com.github.javaparser" % "javaparser-core" % "3.2.5",
      "com.github.scopt" %% "scopt" % "3.5.0" % Test
    ),
    unmanagedSourceDirectories in Compile ++= {
      if (Set("2.12", "2.11").contains(scalaBinaryVersion.value))
        Seq(baseDirectory.value / "src" / "main" / "scala-2.11_2.12")
      else
        Seq()
    },
    unmanagedSourceDirectories in Compile ++= {
      if (Set("2.11").contains(scalaBinaryVersion.value))
        Seq(baseDirectory.value / "src" / "main" / "scala-2.10_2.11")
      else
        Seq()
    },

    // Aggregate source jars into the amm/test:run classpath, so that the
    // `source` macro can find their sources and highlight/display them.
    (fullClasspath in Test) ++= {
      (updateClassifiers in Test).value
        .configurations
        .find(_.configuration == Test.name)
        .get
        .modules
        .flatMap(_.artifacts)
        .collect{case (a, f) if a.classifier == Some("sources") => f}
    }
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
    macroSettings,
    crossVersion := CrossVersion.full,
    name := "ammonite-shell",
    fork in Test := true,
    baseDirectory in Test := (baseDirectory in Test).value / "..",
    envVars in Test := Map(
      "AMMONITE_TEST_SHELL" -> (packageBin in Compile).value.toString,
      "AMMONITE_TEST_ASSEMBLY" -> (assembly in amm).value.toString
    )
  )

lazy val integration = project
  .dependsOn(ops)
  .dependsOn(amm)
  .settings(
    sharedSettings,
    fork in Test := true,
    baseDirectory in Test := (baseDirectory in Test).value / "..",
    envVars in Test := Map(
      "AMMONITE_TEST_SHELL" -> (packageBin in (shell, Compile)).value.toString,
      "AMMONITE_TEST_ASSEMBLY" -> (assembly in amm).value.toString
    ),
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
        // sshd-core 1.3.0 requires java8
        "org.apache.sshd" % "sshd-core" % "1.2.0",
        "org.bouncycastle" % "bcprov-jdk15on" % "1.56",
        //-- test --//
        // slf4j-nop makes sshd server use logger that writes into the void
        "org.slf4j" % "slf4j-nop" % "1.7.12" % Test,
        "com.jcraft" % "jsch" % "0.1.54" % Test,
        "org.scalacheck" %% "scalacheck" % "1.12.6" % Test
      )
  )

lazy val readme = ScalatexReadme(
  projectId = "readme",
  wd = file(""),
  url = "https://github.com/lihaoyi/ammonite/tree/master",
  source = "Index"
).settings(
  dontPublishSettings,
  scalaVersion := "2.12.3",
  libraryDependencies += "com.lihaoyi" %% "fansi" % "0.2.3",
  (run in Compile) := (run in Compile).dependsOn(
    assembly in (amm, Test),
    packageBin in (shell, Compile),
    doc in (ops, Compile),
    doc in (terminal, Compile),
    doc in (amm, Compile),
    doc in (sshd, Compile),
    doc in (shell, Compile)
  ).evaluated,
  (run in Compile) := (run in Compile).dependsOn(Def.task{
    val apiFolder = (target in Compile).value/"scalatex"/"api"
    val copies = Seq(
      (doc in (ops, Compile)).value -> "ops",
      (doc in (terminal, Compile)).value -> "terminal",
      (doc in (amm, Compile)).value -> "amm",
      (doc in (sshd, Compile)).value -> "sshd",
      (doc in (shell, Compile)).value -> "shell"
    )
    for ((folder, name) <- copies){
      sbt.IO.copyDirectory(folder, apiFolder/name, overwrite = true)
    }
  }).evaluated,
  envVars in Test := Map(
    "AMMONITE_TEST_SHELL" -> (packageBin in Compile).value.toString,
    "AMMONITE_TEST_ASSEMBLY" -> (assembly in amm).value.toString
  ),
  envVars in run := Map(
    "AMMONITE_TEST_SHELL" -> (packageBin in (shell, Compile)).value.toString
  ),
  fork := true,
  baseDirectory in (Compile, run) := (baseDirectory in (Compile, run)).value / "..",
  (unmanagedSources in Compile) += baseDirectory.value/".."/"project"/"Constants.scala"
)

// Only modules down-stream of `ammInterp` need to be fully cross-built against
// minor versions, since `interp` depends on compiler internals. The modules
// upstream of `ammInterp` can be cross-built normally only against major versions
// of Scala
lazy val singleCrossBuilt = project
  .in(file("target/singleCrossBuilt"))
  .aggregate(ops, terminal, ammUtil, ammRuntime)
  .settings(dontPublishSettings)

lazy val fullCrossBuilt = project
  .in(file("target/fullCrossBuilt"))
  .aggregate(shell, amm, sshd, ammInterp, ammRepl)
  .settings(dontPublishSettings)


lazy val published = project
  .in(file("target/published"))
  .aggregate(fullCrossBuilt, singleCrossBuilt)
  .settings(dontPublishSettings)
