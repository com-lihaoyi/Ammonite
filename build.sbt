scalaVersion := "2.11.4"

publishArtifact := false

publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))

val acyclicSettings = Seq(
  autoCompilerPlugins := true,
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided"
  )
)

val publishingSettings = Seq(
  organization := "com.lihaoyi",
  version := "0.2.4",
  scalacOptions += "-target:jvm-1.7",
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

val sharedSettings = Seq(
  scalaVersion := "2.11.5",
  libraryDependencies += "com.lihaoyi" %% "utest" % "0.3.0" % "test",
  testFrameworks += new TestFramework("utest.runner.Framework"),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
  )
) ++ acyclicSettings ++ publishingSettings

lazy val pprint = project
  .settings(sharedSettings:_*)
  .settings(
    name := "ammonite-pprint",
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
      "com.lihaoyi" %% "scala-parser" % "0.1.1"
    ),
    javaOptions += "-Xmx2G",
    fork in (Test, testOnly) := true
  )

lazy val readme = project
  .settings(scalatex.SbtPlugin.projectSettings:_*)
  .settings(
    libraryDependencies += "com.lihaoyi" %% "scalatex-site" % "0.1.4",
    libraryDependencies += "com.lihaoyi" %% "ammonite-ops" % "0.2.4",
    scalaVersion := "2.11.4"
)

lazy val plugin = project.in(file("sbt-plugin")).copy(id = "sbt-plugin")
  .settings(acyclicSettings ++ publishingSettings: _*)
  .settings(
    name := "ammonite-sbt",
    sbtPlugin := true
  )
