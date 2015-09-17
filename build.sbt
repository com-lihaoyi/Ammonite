import scalatex.ScalatexReadme
import sbtassembly.AssemblyPlugin.defaultShellScript

scalaVersion := "2.11.6"

crossScalaVersions := Seq(
  "2.10.3", "2.10.4", "2.10.5", "2.11.3", "2.11.4", "2.11.5", "2.11.6", "2.11.7"
)

val dontPublishSettings = Seq(
  publishArtifact := false,
  publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))
)

dontPublishSettings

val sharedSettings = Seq(
  scalaVersion := "2.11.7",
  organization := "com.lihaoyi",
  version := _root_.ammonite.Constants.version,
  libraryDependencies += "com.lihaoyi" %% "utest" % "0.3.0" % "test",
  testFrameworks += new TestFramework("utest.runner.Framework"),
  scalacOptions += "-target:jvm-1.7",
  autoCompilerPlugins := true,
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
  parallelExecution in Test := !scalaVersion.value.contains("2.10"),
  (unmanagedSources in Compile) += baseDirectory.value/".."/"project"/"Constants.scala",
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
  ) ++ (
    if (scalaVersion.value startsWith "2.11.") Nil
    else Seq(
      compilerPlugin("org.scalamacros" % s"paradise" % "2.0.1" cross CrossVersion.full),
      "org.scalamacros" %% s"quasiquotes" % "2.0.1"
    )
  ),
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
  .settings(sharedSettings:_*)
  .settings(
    name := "ammonite-ops",
    libraryDependencies += "com.lihaoyi" %% "pprint" % "0.3.4"
  )


/**
 * A standalone re-implementation of a composable readline-style REPL,
 * without any behavior associated with it. Contains an echo-repl that
 * can be run to test the REPL interactions
 */
lazy val terminal = project
  .settings(sharedSettings:_*)
  .settings(
    name := "ammonite-terminal"
  )

/**
 * A better Scala REPL, which can be dropped in into any project or run
 * standalone in a Scala project to provide a better interactive experience
 * for Scala
 */
lazy val repl = project
  .dependsOn(terminal, ops)
  .settings(sharedSettings:_*)
  .settings(
    crossVersion := CrossVersion.full,
    test in assembly := {},
    name := "ammonite-repl",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "jline" % "jline" % "2.12",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.apache.ivy" % "ivy" % "2.4.0",
      "com.lihaoyi" %% "scalaparse" % "0.2.1",
      "com.lihaoyi" %% "upickle" % "0.3.6",
      "com.lihaoyi" %% "pprint" % "0.3.6",
      "com.github.scopt" %% "scopt" % "3.3.0"
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
    assemblyJarName in assembly := s"${name.value}-${version.value}-${scalaVersion.value}",
    sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
      val file = dir/"ammonite"/"pprint"/"PPrintGen.scala"

      val typeGen = for(i <- 2 to 22) yield {
        val ts = (1 to i).map("T" + _).mkString(", ")
        val tsBounded = (1 to i).map("T" + _ + ": Type").mkString(", ")
        val tsGet = (1 to i).map("get[T" + _ + "](cfg)").mkString(" + \", \" + ")
        s"""
          implicit def F${i}TPrint[$tsBounded, R: Type] = make[($ts) => R](cfg =>
            "(" + $tsGet + ") => " + get[R](cfg)
          )
          implicit def T${i}TPrint[$tsBounded] = make[($ts)](cfg =>
            "(" + $tsGet + ")"
          )

        """
      }
      val output = s"""
        package ammonite.repl.frontend

        trait TPrintGen[Type[_], Cfg]{
          def make[T](f: Cfg => String): Type[T]
          def get[T: Type](cfg: Cfg): String
          implicit def F0TPrint[R: Type] = make[() => R](cfg => "() => " + get[R](cfg))
          implicit def F1TPrint[T1: Type, R: Type] = {
            make[T1 => R](cfg => get[T1](cfg) + " => " + get[R](cfg))
          }
          ${typeGen.mkString("\n")}
        }
      """.stripMargin
      IO.write(file, output)
      Seq(file)
    }
  )

/**
 * Project that binds together [[ops]] and [[repl]], turning them into a
 * credible systems shell that can be used to replace bash/zsh/etc. for
 * common housekeeping tasks
 */
lazy val shell = project
  .dependsOn(ops, repl % "compile->compile;test->test")
  .settings(sharedSettings:_*)
  .settings(
    name := "ammonite-shell",
    libraryDependencies += "com.lihaoyi" %% "pprint" % "0.3.4",
    (test in Test) <<= (test in Test).dependsOn(publishLocal),
    (testOnly in Test) <<= (testOnly in Test).dependsOn(publishLocal)
  )

lazy val integration = project
  .dependsOn(ops)
  .settings(sharedSettings:_*)
  .settings(
    (test in Test) <<= (test in Test).dependsOn(assembly in repl, publishLocal in published),
    (testOnly in Test) <<= (testOnly in Test).dependsOn(assembly in repl, publishLocal in published),
    dontPublishSettings
  )


/**
 * REPL available via remote ssh access.
 * Plug into any app environment for live hacking on a live application.
 */
lazy val sshd = project.
    dependsOn(repl).
    settings(sharedSettings:_*).
    settings(
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
  scalaVersion := "2.11.7",
  (run in Compile) <<= (run in Compile).dependsOn(publishLocal in published, assembly in repl),
  (unmanagedSources in Compile) += baseDirectory.value/".."/"project"/"Constants.scala"
)


lazy val tested = project
  .aggregate(published, integration)
  .settings(dontPublishSettings)

lazy val published = project
  .aggregate(ops, shell, terminal, repl, sshd)
  .settings(dontPublishSettings)
