import scalatex.ScalatexReadme
import sbtassembly.AssemblyPlugin.defaultShellScript

lazy val sharedSettings = Seq(
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq(
    "2.10.3", "2.10.4", "2.10.5", "2.11.3", "2.11.4", "2.11.5", "2.11.6", "2.11.7"
  ),
  libraryDependencies += "com.lihaoyi" %% "utest" % "0.3.0" % "test",
  testFrameworks += new TestFramework("utest.runner.Framework"),
  scalacOptions += "-target:jvm-1.7",
  parallelExecution in Test := !scalaVersion.value.contains("2.10"),
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

lazy val acyclicSettings = Seq(
  autoCompilerPlugins := true,
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
  libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided"
)

lazy val publishSettings = Seq(
  organization := "com.lihaoyi",
  version := _root_.ammonite.Constants.version,
  publishTo := Some("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"),
  pomIncludeRepository := { _ => false },
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

lazy val noPublishSettings = Seq(
  publish := (),
  publishLocal := (),
  publishArtifact := false
)

/**
 * Concise, type-safe operating-system operations in Scala: filesystem,
 * subprocesses, and other such things.
 */
lazy val ops = project
  .settings(sharedSettings)
  .settings(acyclicSettings)
  .settings(publishSettings)
  .settings(
    name := "ammonite-ops",
    libraryDependencies += "com.lihaoyi" %% "pprint" % "0.3.4"
  )

lazy val tools = project
  .dependsOn(ops)
  .settings(sharedSettings)
  .settings(acyclicSettings)
  .settings(publishSettings)
  .settings(
    name := "ammonite-tools",
    libraryDependencies += "com.lihaoyi" %% "pprint" % "0.3.4"
  )

/**
 * A standalone re-implementation of a composable readline-style REPL,
 * without any behavior associated with it. Contains an echo-repl that
 * can be run to test the REPL interactions
 */
lazy val terminal = project
  .settings(sharedSettings)
  .settings(acyclicSettings)
  .settings(publishSettings)
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
  .settings(sharedSettings)
  .settings(acyclicSettings)
  .settings(publishSettings)
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
      "com.lihaoyi" %% "upickle" % "0.3.5",
      "com.lihaoyi" %% "pprint" % "0.3.5",
      "com.github.scopt" %% "scopt" % "3.3.0"
    ),
    libraryDependencies ++= (
      if (scalaVersion.value startsWith "2.10.") Nil
      else Seq("com.chuusai" %% "shapeless" % "2.1.0" % "test")
    ),
    (unmanagedSources in Compile) += baseDirectory.value/".."/"project"/"Constants.scala",
    javaOptions += "-Xmx4G",
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(
      prependShellScript = Some(defaultShellScript)
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
          implicit def F1TPrint[T1: Type, R: Type] = make[T1 => R](cfg => get[T1](cfg) + " => " + get[R](cfg))
          ${typeGen.mkString("\n")}
        }
      """.stripMargin
      IO.write(file, output)
      Seq(file)
    }


//    fork in (Test, testOnly) := true
  )

lazy val readme = ScalatexReadme(
  projectId = "readme",
  wd = file(""),
  url = "https://github.com/lihaoyi/ammonite/tree/master",
  source = "Index"
).settings(
  publishArtifact := false,
  publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo"))),
  (unmanagedSources in Compile) += baseDirectory.value/".."/"project"/"Constants.scala"
)

lazy val mainProjects = Seq[ProjectReference](
  ops, tools, terminal, repl
)

lazy val modules = project
  .aggregate(mainProjects: _*)
  .settings(sharedSettings)
  .settings(noPublishSettings)

lazy val root = project.in(file("."))
  .aggregate(mainProjects: _*)
  .dependsOn(mainProjects.map(p => p: ClasspathDep[ProjectReference]): _*)
  .settings(sharedSettings)
  .settings(noPublishSettings)
