scalaVersion := "2.11.5"

crossScalaVersions := Seq("2.11.5", "2.10.5")

publishArtifact := false

publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))

val sharedSettings = Seq(
  scalaVersion := "2.11.5",
  crossScalaVersions := Seq("2.11.5", "2.10.5"),
  organization := "com.lihaoyi",
  version := "0.2.7",
  libraryDependencies += "com.lihaoyi" %% "utest" % "0.3.0" % "test",
  testFrameworks += new TestFramework("utest.runner.Framework"),
  scalacOptions += "-target:jvm-1.7",
  autoCompilerPlugins := true,
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
  ),
  publishTo := Some("releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2"),
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

lazy val pprint = project
  .settings(sharedSettings:_*)
  .settings(
    name := "ammonite-pprint",
    libraryDependencies ++= {
      if (scalaVersion.value startsWith "2.10.")
        Seq(
          "org.scalamacros" %% "quasiquotes" % "2.0.1",
          compilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
        )
      else
        Seq()
    },
    sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
      val file = dir/"ammonite"/"pprint"/"PPrintGen.scala"
      val tuples = (1 to 22).map{ i =>
        val ts = (1 to i) map ("T" + _)
        val chunks = 1 to i map { n =>
          s"render(t._$n, cfg)"
        }
        val commaTs = ts.mkString(", ")
        val tupleType = s"Product$i[$commaTs]"
        val boundedTypes = ts.map(_ + ": PP").mkString(",")
        s"""
        implicit def Product${i}Unpacker[$boundedTypes] = {
          (t: $tupleType, cfg: C) => Iterator(${chunks.mkString(",")})
        }
        """
      }
      val output = s"""
        package ammonite.pprint
        trait PPrinterGen extends GenUtils{
          ${tuples.mkString("\n")}
        }
      """.stripMargin
      IO.write(file, output)
      Seq(file)
    }
  )

lazy val ops = project
  .dependsOn(pprint)
  .settings(sharedSettings:_*)
  .settings(
    name := "ammonite-ops"
  )

lazy val tools = project
  .dependsOn(ops, pprint)
  .settings(sharedSettings:_*)
  .settings(
    name := "ammonite-tools"
  )

lazy val repl = project
  .dependsOn(ops, tools, pprint)
  .settings(sharedSettings:_*)
  .settings(
    test in assembly := {},
    name := "ammonite-repl",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "jline" % "jline" % "2.12",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.apache.ivy" % "ivy" % "2.4.0",
      "com.lihaoyi" %% "scala-parser" % "0.1.3"
    ),
    javaOptions += "-Xmx2G",
    fork in (Test, testOnly) := true,
    // Will not be necessary with sbt 0.13.8
    unmanagedSourceDirectories in Compile += (sourceDirectory in Compile).value / s"scala-${scalaBinaryVersion.value}"
  )

lazy val readme = project
  .settings(scalatex.SbtPlugin.projectSettings:_*)
  .settings(
    libraryDependencies += "com.lihaoyi" %% "scalatex-site" % "0.1.4",
    libraryDependencies += "com.lihaoyi" %% "ammonite-ops" % "0.2.4",
    scalaVersion := "2.11.4"
)

lazy val root = project.in(file(".")).aggregate(pprint,  ops, tools, repl)