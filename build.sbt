
import scalatex.ScalatexReadme
import sbtassembly.AssemblyPlugin.defaultShellScript
scalaVersion := "2.11.6"

crossScalaVersions := Seq(
  "2.11.3", "2.11.4", "2.11.5", "2.11.6", "2.11.7"
)

publishArtifact := false

publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo")))

val sharedSettings = Seq(
  scalaVersion := "2.11.7",
  organization := "com.lihaoyi",
  version := _root_.ammonite.Constants.version,
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
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",
      "com.chuusai" %% "shapeless" % "2.1.0" % "test"
    ),
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
        package ammonite.pprint
        trait PPrinterGen extends GenUtils{
          ${tuples.mkString("\n")}
        }
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
  )

/**
 * Concise, type-safe operating-system operations in Scala: filesystem,
 * subprocesses, and other such things.
 */
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
  .dependsOn(pprint, terminal)
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
      "com.lihaoyi" %% "scalaparse" % "0.1.7",
      "org.yaml" % "snakeyaml" % "1.15"
    ),
    javaOptions += "-Xmx4G",
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(
      prependShellScript = Some(defaultShellScript)
    ),
    assemblyJarName in assembly := s"${name.value}-${version.value}"
//    fork in (Test, testOnly) := true
  )

/**
 * A batteries-included version of [[repl]]. This contains the necessary
 * dependencies and configuration to be used comfortably as an system-shell
 */
lazy val shell = project
  .dependsOn(repl, ops)
  .settings(sharedSettings:_*)
  .settings(
    crossVersion := CrossVersion.full,
    test in assembly := {},
    name := "ammonite-shell",
    assemblyOption in assembly := (assemblyOption in assembly).value.copy(
      prependShellScript = Some(defaultShellScript)
    ),
    assemblyJarName in assembly := s"${name.value}-${version.value}"
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


