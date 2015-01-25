
val sharedSettings = Seq(
  organization := "com.lihaoyi",
  scalaVersion := "2.11.5",
  crossScalaVersions := Seq("2.10.4", "2.11.4"),
  version := "0.1.3",
  libraryDependencies += "com.lihaoyi" %% "utest" % "0.2.4" % "test",
  testFrameworks += new TestFramework("utest.runner.JvmFramework"),
  autoCompilerPlugins := true,
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
  ) ++ (
    if (scalaVersion.value startsWith "2.11.") Nil
    else Seq(
      "org.scalamacros" %% s"quasiquotes" % "2.0.0" % "provided",
      compilerPlugin("org.scalamacros" % s"paradise" % "2.0.0" cross CrossVersion.full)
    )
  ),
  publishTo := Some("releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"),
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

lazy val sh = project.dependsOn(core).settings(sharedSettings:_*).settings(
  name := "ammonite-sh",
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "jline" % "jline" % "2.12",
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    "com.lihaoyi" %% "scala-parser-lite" % "0.1.0"
  )
)

lazy val core = project.settings(sharedSettings:_*).settings(
  name := "ammonite",
  sourceGenerators in Compile <+= sourceManaged in Compile map { dir =>
    val file = dir / "ammonite" / "PPrintGen.scala"
    val tuples = (1 to 22).map{ i =>
      val ts = (1 to i) map ("T" + _)
      val chunks = 1 to i map { n =>
        s"render(t._$n, c)"
      }
      val commaTs = ts.mkString(", ")
      val tupleType = s"Product${i}[${commaTs}]"
      val boundedTypes = ts.map(_ + ": PP").mkString(",")
      s"""
      implicit def Product${i}Unpacker[${boundedTypes}] = make[$tupleType] {
        (t: $tupleType, c: Config) => {
          def chunks(c: Config) = Seq(${chunks.mkString(",")})
          (chunks(c), () => chunks(c.deeper))
        }
      }
      """
    }
    val output = s"""
      package ammonite

      trait PPrinterGen extends GenUtils{

        ${tuples.mkString("\n")}
      }
    """.stripMargin
    IO.write(file, output)
    Seq(file)
  },
  initialCommands in console := """
    import ammonite._
    var wd = processWorkingDir
  """
)
