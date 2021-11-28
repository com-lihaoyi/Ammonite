import mill._, scalalib._, publish._
import coursier.mavenRepositoryString
import $file.ci.upload

import $ivy.`io.get-coursier::coursier-launcher:2.0.0-RC6-10`

val ghOrg = "com-lihaoyi"
val ghRepo = "Ammonite"
val masterBranch = "master"
val homePage = "https://ammonite.io"

val isMasterCommit =
  sys.env.get("GITHUB_REPOSITORY") == Some(s"${ghOrg}/${ghRepo}") &&
  sys.env.get("GITHUB_REF").exists(x => x.endsWith(s"/${masterBranch}"))

val latestTaggedVersion = os.proc('git, 'describe, "--abbrev=0", "--tags").call().out.trim

val gitHead = os.proc('git, "rev-parse", "HEAD").call().out.trim

val commitsSinceTaggedVersion = {
  os.proc('git, "rev-list", gitHead, "--not", latestTaggedVersion, "--count")
    .call()
    .out
    .trim
    .toInt
}

// When a Scala 3 version is used as cross scala version,
// cross2_3Version sometimes ends up being picked as actual Scala version.
// Modules retaining the Scala 3 version or cross2_3Version can depend on each other, thanks to
// the dotty compatibility.
// Beware that this requires both versions to have the same tasty format
// version. For example, 2.13.4 and 3.0.0-M1 do, while 2.13.4 and 3.0.0-M{2,3}
// don't.
val cross2_3Version = "2.13.6"


// Same as https://github.com/lihaoyi/mill/blob/0.9.3/scalalib/src/Dep.scala/#L55,
// except we also fix the suffix when scalaVersion is like 2.13.x,
// so that downstream Scala 3 project still pick the _2.13 dependency.
def withDottyCompat(dep: Dep, scalaVersion: String): Dep =
  dep.cross match {
    case cross: CrossVersion.Binary if scalaVersion.startsWith("3.") || scalaVersion.startsWith("2.13.") =>
      val compatSuffix = "_2.13"
      dep.copy(cross = CrossVersion.Constant(value = compatSuffix, platformed = dep.cross.platformed))
    case _ => dep
  }

val scala2_12Versions = Seq("2.12.1", "2.12.2", "2.12.3", "2.12.4", "2.12.6", "2.12.7", "2.12.8", "2.12.9", "2.12.10", "2.12.11", "2.12.12", "2.12.13", "2.12.14", "2.12.15")
val scala2_13Versions = Seq("2.13.0", "2.13.1", "2.13.2", "2.13.3", "2.13.4", "2.13.5", "2.13.6", "2.13.7")
val scala3Versions = Seq("3.0.0", "3.0.1", "3.0.2")

val binCrossScalaVersions = Seq(scala2_12Versions.last, scala2_13Versions.last, scala3Versions.last)
def isScala2_12_10OrLater(sv: String): Boolean = {
  (sv.startsWith("2.12.") && sv.stripPrefix("2.12.").length > 1) || (sv.startsWith("2.13.") && sv != "2.13.0")
}
val fullCrossScalaVersions = scala2_12Versions ++ scala2_13Versions ++ scala3Versions

val latestAssemblies = binCrossScalaVersions.map(amm(_).assembly)

println("GITHUB REF " + sys.env.get("GITHUB_REF"))

val (buildVersion, unstable) = scala.util.Try(
  os.proc('git, 'describe, "--exact-match", "--tags", "--always", gitHead)
    .call()
    .out
    .trim
).toOption match{
  case None =>
    val gitHash = os.proc("git", "rev-parse", "--short", "HEAD").call().out.trim
    (s"$latestTaggedVersion-$commitsSinceTaggedVersion-$gitHash", true)
  case Some(tagName) => (tagName, false)
}

val bspVersion = "2.0.0-M6"
val fastparseVersion = "2.3.0"
val scalametaVersion = "4.4.30"

object Deps {
  val acyclic = ivy"com.lihaoyi::acyclic:0.2.0"
  val bsp4j = ivy"ch.epfl.scala:bsp4j:${bspVersion}"
  val bcprovJdk15on = ivy"org.bouncycastle:bcprov-jdk15on:1.56"
  val cask = ivy"com.lihaoyi::cask:0.6.0"
  val coursierInterface = ivy"io.get-coursier:interface:0.0.21"
  val fansi = ivy"com.lihaoyi::fansi:0.2.14"
  val fastparse = ivy"com.lihaoyi::fastparse:$fastparseVersion"
  val javaparserCore = ivy"com.github.javaparser:javaparser-core:3.2.5"
  val javassist = ivy"org.javassist:javassist:3.21.0-GA"
  val jlineJna = ivy"org.jline:jline-terminal-jna:3.14.1"
  val jlineReader = ivy"org.jline:jline-reader:3.14.1"
  val jlineTerminal = ivy"org.jline:jline-terminal:3.14.1"
  val jsch = ivy"com.jcraft:jsch:0.1.54"
  val mainargs = ivy"com.lihaoyi::mainargs:0.2.0"
  val osLib = ivy"com.lihaoyi::os-lib:0.7.8"
  val pprint = ivy"com.lihaoyi::pprint:0.6.6"
  val requests = ivy"com.lihaoyi::requests:0.6.9"
  val scalacheck = ivy"org.scalacheck::scalacheck:1.14.0"
  val scalaCollectionCompat = ivy"org.scala-lang.modules::scala-collection-compat:2.6.0"
  def scalaCompiler(scalaVersion: String) = ivy"org.scala-lang:scala-compiler:${scalaVersion}"
  val scalaJava8Compat = ivy"org.scala-lang.modules::scala-java8-compat:0.9.0"
  val scalaparse = ivy"com.lihaoyi::scalaparse:$fastparseVersion"
  def scalaReflect(scalaVersion: String) = ivy"org.scala-lang:scala-reflect:${scalaVersion}"
  val scalaXml = ivy"org.scala-lang.modules::scala-xml:2.0.1"
  val scalazCore = ivy"org.scalaz::scalaz-core:7.2.27"
  val semanticDbScalac = ivy"org.scalameta:::semanticdb-scalac:$scalametaVersion"
  val shapeless = ivy"com.chuusai::shapeless:2.3.3"
  val slf4jNop = ivy"org.slf4j:slf4j-nop:1.7.12"
  val sourcecode = ivy"com.lihaoyi::sourcecode:0.2.7"
  val sshdCore = ivy"org.apache.sshd:sshd-core:1.2.0"
  val trees = ivy"org.scalameta::trees:$scalametaVersion"
  val upickle = ivy"com.lihaoyi::upickle:1.4.0"
  val utest = ivy"com.lihaoyi::utest:0.7.10"
}

// Adapted from https://github.com/lihaoyi/mill/blob/0.9.3/scalalib/src/MiscModule.scala/#L80-L100
// Compared to the original code, we added the custom Resolver,
// and ensure `scalaVersion()` rather than `crossScalaVersion` is used
// when computing paths, as the former is always a valid Scala version,
// while the latter can be a 3.x version while we compile using Scala 2.x
// (and later rely on dotty compatibility to mix Scala 2 / Scala 3 modules).
trait CrossSbtModule extends mill.scalalib.SbtModule with mill.scalalib.CrossModuleBase { outer =>

  override def sources = T.sources {
    super.sources() ++
      mill.scalalib.CrossModuleBase.scalaVersionPaths(
        scalaVersion(),
        s => millSourcePath / 'src / 'main / s"scala-$s"
      )

  }
  trait Tests extends super.Tests {
    override def millSourcePath = outer.millSourcePath
    override def sources = T.sources {
      super.sources() ++
        mill.scalalib.CrossModuleBase.scalaVersionPaths(
          scalaVersion(),
          s => millSourcePath / 'src / 'test / s"scala-$s"
        )
    }
  }
}

trait AmmInternalModule extends CrossSbtModule{
  def useCrossPrefix = T{
    scala3Versions.contains(crossScalaVersion) && !scala3Versions.contains(scalaVersion())
  }
  def artifactName = T{
    // When crossScalaVersion == scala3, but we are building the current module
    // with Scala 2 anyway (using -Ytasty-reader), we publish this module as
    // ammonite-cross-23-…. Unlike the other Scala 2 modules, the
    // ammonite-cross-23-… ones can depend on Scala 3 dependencies, even
    // though their JARs should be basically the same as their ammonite-…
    // counterparts.
    val prefix = if (useCrossPrefix()) "ammonite-cross-23-" else "ammonite-"
    prefix + millOuterCtx.segments.parts.mkString("-").stripPrefix("amm-")
  }
  def testFramework = "utest.runner.Framework"
  def isScala2 = T { scalaVersion().startsWith("2.") }
  def scalacOptions = T {
    val acyclicOptions =
      if (isScala2()) Seq("-P:acyclic:force")
      else Nil
    val tastyReaderOptions =
      if (scalaVersion() == cross2_3Version) Seq("-Ytasty-reader")
      else Nil
    acyclicOptions ++ tastyReaderOptions
  }
  def compileIvyDeps = T {
    if (isScala2()) Agg(Deps.acyclic)
    else Agg[Dep]()
  }
  def scalacPluginIvyDeps = T {
    if (isScala2()) Agg(Deps.acyclic)
    else Agg[Dep]()
  }
  trait Tests extends super.Tests with TestModule.Utest{
    def ivyDeps = Agg(Deps.utest)
    def forkArgs = Seq("-Xmx2g", "-Dfile.encoding=UTF8")
  }
  def allIvyDeps = T{transitiveIvyDeps() ++ scalaLibraryIvyDeps()}
  def sources = T.sources{
    val sv = scalaVersion()
    val extraDir =
      if (sv.startsWith("2.12.")) {
        val patch = sv.stripPrefix("2.12.").takeWhile(_.isDigit).toInt
        val dirName0 = if (patch <= 8) "scala-2.12.0_8" else "scala-2.12.9+"
        val dirNames1 = if (patch <= 12) Seq("scala-2.12.0_12") else Nil
        val dirNames = Seq(dirName0) ++ dirNames1
        dirNames.map(dirName => PathRef(millSourcePath / "src" / "main" / dirName))
      } else
        Nil

    val extraDir2 =
      if (isScala2())
        Seq(PathRef(
          if (isScala2_12_10OrLater(sv)) millSourcePath / "src" / "main" / "scala-2.12.10-2.13.1+"
          else millSourcePath / "src" / "main" / "scala-not-2.12.10-2.13.1+"
        ))
      else Nil
    val extraDir3 =
      if (isScala2()) {
        val dir =
          if (sv.startsWith("2.13.") && sv != "2.13.0")
            millSourcePath / "src" / "main" / "scala-2.13.1+"
          else if (sv.startsWith("2.12.") && sv.stripPrefix("2.12.").toInt >= 13)
            millSourcePath / "src" / "main" / "scala-2.12.13+"
          else
            millSourcePath / "src" / "main" / "scala-not-2.12.13+-2.13.1+"
        Seq(PathRef(dir))
      } else Nil
    val extraDir4 =
      if (sv.startsWith("2.13.") || sv.startsWith("3."))
        Seq(PathRef(millSourcePath / "src" / "main" / "scala-2.13-or-3"))
      else Nil
    val extraDir5 =
      if (sv.startsWith("3.") && !sv.startsWith("3.0.0"))
        Seq(PathRef(millSourcePath / "src" / "main" / "scala-3.0.1+"))
      else Nil

    super.sources() ++ extraDir ++ extraDir2 ++ extraDir3 ++ extraDir4 ++ extraDir5
  }
  def externalSources = T{
    resolveDeps(allIvyDeps, sources = true)()
  }
  def supports3: Boolean = false
  def scalaVersion = T{
    if (scala3Versions.contains(crossScalaVersion) && !supports3) cross2_3Version
    else crossScalaVersion
  }
  def repositories = super.repositories ++ Seq(
    mvn"https://scala-ci.typesafe.com/artifactory/scala-integration"
  )
}
trait AmmModule extends AmmInternalModule with PublishModule{
  def publishVersion = buildVersion
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "com.lihaoyi",
    url = s"https://github.com/${ghOrg}/${ghRepo}",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github(ghOrg, ghRepo),
    developers = Seq(
      Developer("lihaoyi", "Li Haoyi","https://github.com/lihaoyi")
    )
  )

  def transitiveJars: T[Agg[PathRef]] = T{
    mill.define.Target.traverse(this +: moduleDeps)(m =>
      T.task{m.jar()}
    )()
  }

  def transitiveSourceJars: T[Agg[PathRef]] = T{
    mill.define.Target.traverse(this +: moduleDeps)(m =>
      T.task{m.sourceJar()}
    )()
  }

}
trait AmmDependenciesResourceFileModule extends JavaModule{
  def dependencyResourceFileName: String
  def dependencyFileResources = T{
    val deps0 = T.task{compileIvyDeps() ++ transitiveIvyDeps()}()
    val (_, res) = mill.modules.Jvm.resolveDependenciesMetadata(
      repositoriesTask(),
      deps0.map(resolveCoursierDependency().apply(_)),
      deps0.filter(_.force).map(resolveCoursierDependency().apply(_)),
      mapDependencies = Some(mapDependencies())
    )


    Seq(PathRef(generateDependenciesFile(
      dependencyResourceFileName,
      res.minDependencies.toSeq
    )))
  }

  def resources = T.sources {
    super.resources() ++ dependencyFileResources()
  }
}

object terminal extends Cross[TerminalModule](binCrossScalaVersions:_*)
class TerminalModule(val crossScalaVersion: String) extends AmmModule{
  def ivyDeps = T{
    val fansi =
      if (scala3Versions.contains(crossScalaVersion))
        Agg(
          ivy"com.lihaoyi:fansi_3:${Deps.fansi.dep.version}",
          ivy"org.scala-lang:scala3-library_3:$crossScalaVersion"
        )
      else
        Agg(Deps.fansi)
    fansi ++ Agg(
      Deps.sourcecode
    )
  }
  def compileIvyDeps = Agg(
    Deps.scalaReflect(scalaVersion())
  )

  object test extends Tests
}

object amm extends Cross[MainModule](fullCrossScalaVersions:_*){
  object util extends Cross[UtilModule](binCrossScalaVersions:_*)
  class UtilModule(val crossScalaVersion: String) extends AmmModule{
    def moduleDeps = Seq()
    def ivyDeps = T{
      Agg(

        Deps.osLib,
        Deps.scalaCollectionCompat,
        Deps.fansi
      ) ++ (
        if (scala3Versions.contains(crossScalaVersion))
          Agg(
            ivy"com.lihaoyi:pprint_3:${Deps.pprint.dep.version}",
            ivy"org.scala-lang:scala3-library_3:$crossScalaVersion"
          )
        else Agg(
          Deps.pprint
        )
      )
    }
    def compileIvyDeps = Agg(
      Deps.scalaReflect(scalaVersion())
    )
  }

  object runtime extends Cross[RuntimeModule](fullCrossScalaVersions:_*)
  class RuntimeModule(val crossScalaVersion: String) extends AmmModule{
    def moduleDeps = Seq(amm.util(), interp.api(), amm.repl.api())
    def crossFullScalaVersion = true
    def ivyDeps = Agg(
      Deps.upickle,
      Deps.requests,
      withDottyCompat(Deps.mainargs, scalaVersion())
    )
  }

  object compiler extends Cross[CompilerModule](fullCrossScalaVersions:_*) {
    object interface extends Cross[CompilerInterfaceModule](fullCrossScalaVersions:_*)
    class CompilerInterfaceModule(val crossScalaVersion: String) extends AmmModule{
      def artifactName = T{
        if (useCrossPrefix()) "ammonite-cross-23-compiler-interface"
        else "ammonite-compiler-interface"
      }
      def crossFullScalaVersion = true
      def moduleDeps = Seq(amm.util())
      def exposedClassPath = T{
        runClasspath() ++
          externalSources() ++
          transitiveJars() ++
          transitiveSourceJars()
      }
    }
  }
  class CompilerModule(val crossScalaVersion: String) extends AmmModule{
    def supports3 = true
    def moduleDeps = Seq(amm.compiler.interface(), amm.util(), amm.repl.api())
    def crossFullScalaVersion = true
    def ivyDeps = T {
      val scalaSpecificDeps =
        if (isScala2())
          Agg(
            Deps.scalaCompiler(scalaVersion()),
            Deps.scalaparse,
            Deps.scalaXml,
          )
        else
          Agg[Dep](
            ivy"org.scala-lang:scala3-compiler_3:${scalaVersion()}"
          )
        scalaSpecificDeps ++ Agg(
          Deps.javassist,
          Deps.javaparserCore
        )
    }

    def exposedClassPath = T{
      runClasspath() ++
        externalSources() ++
        transitiveJars() ++
        transitiveSourceJars()
    }

    object test extends Tests
  }

  object interp extends Cross[InterpModule](fullCrossScalaVersions:_*){
    object api extends Cross[InterpApiModule](fullCrossScalaVersions:_*)
    class InterpApiModule(val crossScalaVersion: String) extends AmmModule with AmmDependenciesResourceFileModule{
      def moduleDeps = Seq(amm.compiler.interface(), amm.util())
      def crossFullScalaVersion = true
      def dependencyResourceFileName = "amm-interp-api-dependencies.txt"
      def ivyDeps = Agg(
        Deps.scalaReflect(scalaVersion()),
        Deps.coursierInterface
      )
      def constantsFile = T {
        val dest = T.dest / "Constants.scala"
        val code =
          s"""package ammonite.interp.script
             |
             |/** Build-time constants. Generated by mill. */
             |object Constants {
             |  def semanticDbVersion = "${Deps.semanticDbScalac.dep.version}"
             |}
             |""".stripMargin
             os.write(dest, code)
        PathRef(dest)
      }
      override def generatedSources: T[Seq[PathRef]] = super.generatedSources() ++ Seq(constantsFile())
    }
  }
  class InterpModule(val crossScalaVersion: String) extends AmmModule{
    def moduleDeps = Seq(amm.util(), amm.runtime(), amm.compiler.interface())
    def crossFullScalaVersion = true
    def ivyDeps = Agg(
      Deps.bsp4j,
      withDottyCompat(Deps.fastparse, scalaVersion()),
      withDottyCompat(Deps.trees, scalaVersion()),
      Deps.scalaReflect(scalaVersion()),
      Deps.scalaXml
    )
  }

//  object `test-runner` extends mill.scalalib.SbtModule {
//    def scalaVersion = "2.12.8"
//    def ivyDeps = Agg(
//      ivy"com.lihaoyi::mill-scalalib:${sys.props("MILL_VERSION")}"
//    )
//  }

  object repl extends Cross[ReplModule](fullCrossScalaVersions:_*){

    object api extends Cross[ReplApiModule](fullCrossScalaVersions:_*)
    class ReplApiModule(val crossScalaVersion: String) extends AmmModule with AmmDependenciesResourceFileModule{
      def crossFullScalaVersion = true
      def dependencyResourceFileName = "amm-dependencies.txt"
      def moduleDeps = Seq(amm.util(), interp.api())
      def ivyDeps = Agg(
        withDottyCompat(Deps.mainargs, scalaVersion())
      )

      def generatedSources = T{
        Seq(PathRef(generateConstantsFile(buildVersion, bspVersion = bspVersion)))
      }

      def exposedClassPath = T{
        amm.repl.api().runClasspath() ++
          amm.repl.api().externalSources() ++
          amm.repl.api().transitiveJars() ++
          amm.repl.api().transitiveSourceJars()
      }
    }

  }
  class ReplModule(val crossScalaVersion: String) extends AmmModule{
    def crossFullScalaVersion = true
    def moduleDeps = Seq(
      amm.util(),
      amm.runtime(), amm.interp(),
      terminal(),
      amm.compiler.interface()
    )
    def ivyDeps = Agg(
      Deps.jlineTerminal,
      Deps.jlineJna,
      Deps.jlineReader
    )

    object test extends Tests with AmmDependenciesResourceFileModule {
      def crossScalaVersion = ReplModule.this.crossScalaVersion
      def scalaVersion = ReplModule.this.scalaVersion
      def dependencyResourceFileName = "amm-test-dependencies.txt"
      def moduleDeps = super.moduleDeps ++ Seq(amm.compiler())

      def thinWhitelist = T{
        generateApiWhitelist(
          amm.repl.api().exposedClassPath() ++
          amm.compiler().exposedClassPath() ++
          Seq(compile().classes) ++
          resolveDeps(T.task{compileIvyDeps() ++ transitiveIvyDeps()})()
        )
      }

      def localClasspath = T{
        super.localClasspath() ++ Agg(thinWhitelist())
      }

      def resources = T.sources {
        (super.resources() ++
        ReplModule.this.sources() ++
        ReplModule.this.externalSources() ++
        resolveDeps(ivyDeps, sources = true)()).distinct
      }
      def ivyDeps = super.ivyDeps() ++ amm.compiler().ivyDeps() ++ Agg(
        withDottyCompat(Deps.scalazCore, scalaVersion())
      )
    }
  }

  // When built with crossScalaVersion == 3.x, amm itself is still compiled with
  // Scala 2 (using dotty compatibility mode), and is published as
  // com.lihaoyi:ammonite-cross-23_2.13.x, in order not to conflict with the main
  // Scala 2.13.x amm module. In order to still publish a com.lihaoyi:ammonite_3.x module,
  // we build an empty module, and have it depend on com.lihaoyi:ammonite-cross-23_2.13.x.
  object helper extends Cross[HelperModule](scala3Versions: _*)
  class HelperModule(val crossScalaVersion: String) extends AmmModule {
    def supports3 = true
    def artifactName = "ammonite"
    def crossFullScalaVersion = true
    def mainClass = Some("ammonite.Main")
    def moduleDeps = Seq(amm())
  }
}

class MainModule(val crossScalaVersion: String)
  extends AmmModule {

  def artifactName = T{
    // See AmmInternalModule.artifactName for more details about ammonite-cross-23.
    if (useCrossPrefix()) "ammonite-cross-23"
    else "ammonite"
  }

  def crossFullScalaVersion = true

  def mainClass = Some("ammonite.Main")

  def moduleDeps = Seq(
    terminal(),
    amm.util(), amm.runtime(),
    amm.interp.api(),
    amm.repl.api(),
    amm.interp(), amm.repl(),
    amm.compiler()
  )

  def runClasspath =
    super.runClasspath() ++
    terminal().sources() ++
    amm.util().sources() ++
    amm.runtime().sources() ++
    amm.interp.api().sources() ++
    amm.repl.api().sources() ++
    amm.interp().sources() ++
    amm.repl().sources() ++
    sources() ++
    externalSources()

  def prependShellScript = T{
    mill.modules.Jvm.launcherUniversalScript(
      mainClass().get,
      Agg("$0"),
      Agg("%~dpnx0"),
      // G1 Garbage Collector is awesome https://github.com/lihaoyi/Ammonite/issues/216
      Seq("-Xmx500m", "-XX:+UseG1GC")
    )
  }

  def thinWhitelist = T{
    generateApiWhitelist(
      amm.repl.api().exposedClassPath() ++
      amm.compiler().exposedClassPath()
    )
  }
  def localClasspath = T{
    super.localClasspath() ++ Agg(thinWhitelist())
  }

  def launcher = {
    val isWindows = scala.util.Properties.isWin
    if (isWindows)
      T{
        val mainClass = finalMainClass()
        val cp = runClasspath().map(_.path)
        val jvmOpts = forkArgs()
        val dest = T.ctx().dest / "run.bat"

        import coursier.launcher.{BootstrapGenerator, ClassLoaderContent, Parameters, Preamble}
        val classLoaderContent = ClassLoaderContent.fromUrls(cp.map(_.toNIO.toUri.toASCIIString))
        val params = Parameters.Bootstrap(Seq(classLoaderContent), mainClass)
          .withPreamble(
            Preamble()
              .withKind(Preamble.Kind.Bat)
              .withJavaOpts(jvmOpts)
          )
        val thread = Thread.currentThread()
        val cl = thread.getContextClassLoader
        try {
          thread.setContextClassLoader(BootstrapGenerator.getClass.getClassLoader)
          BootstrapGenerator.generate(params, dest.toNIO)
        } finally {
          thread.setContextClassLoader(cl)
        }

        PathRef(dest)
      }
    else
      T{
        super.launcher()
      }
  }

  object test extends Tests{
    def moduleDeps = super.moduleDeps ++ Seq(amm.compiler().test, amm.repl().test)
    def ivyDeps = super.ivyDeps() ++ Agg(
      Deps.shapeless,
      Deps.scalaJava8Compat
    )


    def thinWhitelist = T{
      generateApiWhitelist(
        amm.repl.api().exposedClassPath() ++
        amm.compiler().exposedClassPath() ++
        Seq(amm.repl().test.compile().classes, compile().classes) ++
        resolveDeps(T.task{compileIvyDeps() ++ transitiveIvyDeps()})()
      )
    }

    def localClasspath = T{
      super.localClasspath() ++ Agg(thinWhitelist())
    }

    // Need to duplicate this from MainModule due to Mill not properly propagating it through
    def runClasspath =
      super.runClasspath() ++
      terminal().sources() ++
      amm.util().sources() ++
      amm.runtime().sources() ++
      amm.interp.api().sources() ++
      amm.repl.api().sources() ++
      amm.interp().sources() ++
      amm.repl().sources() ++
      sources() ++
      externalSources()

  }
}

def generateApiWhitelist(replApiCp: Seq[PathRef])(implicit ctx: mill.api.Ctx.Dest) = {

  val thinClasspathEntries = replApiCp.map(_.path).flatMap{ cpRoot =>
    if (os.isFile(cpRoot) && cpRoot.ext == "jar") {
      val zip = new java.util.zip.ZipFile(cpRoot.toIO)
      import collection.JavaConverters._
      for(e <- zip.entries().asScala) yield e.getName
    }
    else if (os.isDir(cpRoot)) {
      for(sub <- os.walk(cpRoot)) yield sub.relativeTo(cpRoot).toString
    }
    else if (!os.exists(cpRoot)) Nil
    else throw new Exception(cpRoot.toString)
  }
  os.write(
    ctx.dest / "ammonite-api-whitelist.txt",
    thinClasspathEntries
      .flatMap(_.stripSuffix("/").split('/').inits)
      .filter(_.nonEmpty)
      .map(_.mkString("/"))
      .distinct
      .mkString("\n")
  )
  PathRef(ctx.dest)
}

object integration extends Cross[IntegrationModule](fullCrossScalaVersions:_*)
class IntegrationModule(val crossScalaVersion: String) extends AmmInternalModule{
  def moduleDeps = Seq(amm())
  def ivyDeps = T{
    if (scalaVersion().startsWith("2.13."))
      Agg(Deps.cask)
    else
      Agg.empty
  }
  object test extends Tests {
    def forkEnv = super.forkEnv() ++ Seq(
      "AMMONITE_ASSEMBLY" -> amm().launcher().path.toString
    )
  }
}

object sshd extends Cross[SshdModule](fullCrossScalaVersions:_*)
class SshdModule(val crossScalaVersion: String) extends AmmModule{
  def moduleDeps = Seq(amm())
  def crossFullScalaVersion = true
  def ivyDeps = Agg(
    // sshd-core 1.3.0 requires java8
    Deps.sshdCore,
    Deps.bcprovJdk15on
  )
  object test extends Tests {
    def ivyDeps = super.ivyDeps() ++ Agg(
      // slf4j-nop makes sshd server use logger that writes into the void
      Deps.slf4jNop,
      Deps.jsch,
      withDottyCompat(Deps.scalacheck, scalaVersion())
    )
  }
}

def unitTest(scalaVersion: String = sys.env("TRAVIS_SCALA_VERSION")) = T.command{
  terminal(scalaVersion).test.test()()
  amm.repl(scalaVersion).test.test()()
  amm(scalaVersion).test.test()()
  sshd(scalaVersion).test.test()()
}

def integrationTest(scalaVersion: String = sys.env("TRAVIS_SCALA_VERSION")) = T.command{
  integration(scalaVersion).test.test()()
}

def generateConstantsFile(version: String = buildVersion,
                          unstableVersion: String = "<fill-me-in-in-Constants.scala>",
                          bspVersion: String = "<fill-me-in-in-Constants.scala>",
                          curlUrl: String = "<fill-me-in-in-Constants.scala>",
                          unstableCurlUrl: String = "<fill-me-in-in-Constants.scala>",
                          oldCurlUrls: Seq[(String, String)] = Nil,
                          oldUnstableCurlUrls: Seq[(String, String)] = Nil)
                         (implicit ctx: mill.util.Ctx.Dest)= {
  val versionTxt = s"""
    package ammonite
    object Constants{
      val version = "$version"
      val unstableVersion = "$unstableVersion"
      val bspVersion = "$bspVersion"
      val curlUrl = "$curlUrl"
      val unstableCurlUrl = "$unstableCurlUrl"
      val oldCurlUrls = Seq[(String, String)](
        ${oldCurlUrls.map{case (name, value) => s""" "$name" -> "$value" """}.mkString(",\n")}
      )
      val oldUnstableCurlUrls = Seq[(String, String)](
        ${oldUnstableCurlUrls.map{case (name, value) => s""" "$name" -> "$value" """}.mkString(",\n")}
      )
    }
  """
  println("Writing Constants.scala")

  os.write(ctx.dest/"Constants.scala", versionTxt)
  ctx.dest/"Constants.scala"
}

def generateDependenciesFile(fileName: String,
                             deps: Seq[coursier.Dependency])
                            (implicit ctx: mill.util.Ctx.Dest) = {

  val dir = ctx.dest / "extra-resources"
  val dest = dir / fileName

  val content = deps
    .map { dep =>
      (dep.module.organization.value, dep.module.name.value, dep.version)
    }
    .sorted
    .map {
      case (org, name, ver) =>
        s"$org:$name:$ver"
    }
    .mkString("\n")

  os.makeDir(dir)
  println(s"Writing $dest")
  dir.toIO.mkdirs()
  os.write(dest, content.getBytes("UTF-8"))

  dir
}


def publishExecutable() = {
  if (!isMasterCommit) T.command{
    println("MISC COMMIT: generating executable but not publishing")
    mill.define.Task.sequence(latestAssemblies)()
  }else T.command{
    val latestAssemblyJars = mill.define.Task.sequence(latestAssemblies)()

    println("MASTER COMMIT: Creating a release")
    if (!unstable){
      requests.post(
        s"https://api.github.com/repos/${ghOrg}/${ghRepo}/releases",
        data = ujson.write(
          ujson.Obj(
            "tag_name" -> buildVersion,
            "name" -> buildVersion,
            "body" -> s"${homePage}/#${buildVersion}"
          )
        ),
        headers = Seq("Authorization" -> s"token ${sys.env("AMMONITE_BOT_AUTH_TOKEN")}")
      )
    }

    for ((version, jar) <- binCrossScalaVersions.zip(latestAssemblyJars)) {
      println("MASTER COMMIT: Publishing Executable for Scala " + version)
      //Prepare executable

      val scalaBinaryVersion = version.take(version.lastIndexOf("."))
      upload(
        uploadedFile = jar.path,
        tagName = latestTaggedVersion,
        uploadName = s"$scalaBinaryVersion-$buildVersion",
        authKey = sys.env("AMMONITE_BOT_AUTH_TOKEN"),
        ghOrg = ghOrg,
        ghRepo = ghRepo
      )
      upload(
        uploadedFile = os.temp(
          os.read(os.pwd / "amm-template.sh")
            .replace("DEFAULT_AMM_VERSION=", s"DEFAULT_AMM_VERSION=$latestTaggedVersion")
            .replace("SCALA_VERSION=", s"SCALA_VERSION=$scalaBinaryVersion")
        ),
        tagName = latestTaggedVersion,
        uploadName = s"$scalaBinaryVersion-$buildVersion-bootstrap",
        authKey = sys.env("AMMONITE_BOT_AUTH_TOKEN"),
        ghOrg = ghOrg,
        ghRepo = ghRepo
      )
    }
  }
}

def publishDocs() = {
  // Disable doc auto-publishing for now, as the recent modularization means we
  // need to make significant changes to the readme and that'll time.
  if (!isMasterCommit) T.command{
    println("MISC COMMIT: Building readme for verification")
    try {
      os.proc(
        "sbt",
        "readme/run",
      ).call(
        env = Map(
          "AMMONITE_ASSEMBLY" -> amm("2.13.1").assembly().path.toString,
          "CONSTANTS_FILE" -> generateConstantsFile().toString
        )
      )
    }catch{case e =>
      println(e)
      e.printStackTrace()
      throw e
    }
  }else T.command{
    println("MASTER COMMIT: Updating version and publishing to Github Pages")

    val deployKey = sys.env("DEPLOY_KEY").replace("\\n", "\n")
    os.write(os.pwd / 'deploy_key, deployKey)

    val (stableKey, unstableKey, oldStableKeys, oldUnstableKeys) =
      if (!unstable){
        (
          s"$latestTaggedVersion/2.13-$latestTaggedVersion",
          s"$latestTaggedVersion/2.13-$latestTaggedVersion",
          for(v <- Seq("2.12"))
            yield s"$latestTaggedVersion/$v-$latestTaggedVersion",
          for(v <- Seq("2.12"))
            yield s"$latestTaggedVersion/$v-$latestTaggedVersion"
        )
      }else{
        (
          s"$latestTaggedVersion/2.13-$latestTaggedVersion",
          s"$latestTaggedVersion/2.13-$buildVersion",
          for(v <- Seq("2.12"))
            yield s"$latestTaggedVersion/$v-$latestTaggedVersion",
          for(v <- Seq("2.12"))
            yield s"$latestTaggedVersion/$v-$buildVersion"
        )
      }
    println("(stableKey, unstableKey)")
    println((stableKey, unstableKey))
    val constantsFile = generateConstantsFile(
      latestTaggedVersion,
      buildVersion,
      bspVersion,
      s"https://github.com/${ghOrg}/${ghRepo}/releases/download/$stableKey",
      s"https://github.com/${ghOrg}/${ghRepo}/releases/download/$unstableKey",
      for(k <- oldStableKeys)
        yield (k, s"https://github.com/${ghOrg}/${ghRepo}/releases/download/$k"),
      for(k <- oldUnstableKeys)
        yield (k, s"https://github.com/${ghOrg}/${ghRepo}/releases/download/$k")
    )

    os.proc(
      "readme/run",

    ).call(
      env = Map(
        "AMMONITE_ASSEMBLY" -> amm("2.13.1").assembly().path.toString,
        "CONSTANTS_FILE" -> constantsFile.toString
      )
    )
    os.proc("ci/deploy_master_docs.sh").call()
  }
}

def partition(publishArtifacts: mill.main.Tasks[PublishModule.PublishData],
              shard: Int,
              divisionCount: Int) = {

  val groupedArtifacts = publishArtifacts.value
    .map{ t =>
      val taskCrossVersion = t.ctx.segments.value
        .collectFirst{ case mill.define.Segment.Cross(List(v)) => v }
        .get

      // Sort primarily on the scalaVersion, using the rendered name of the
      // task as the secondary sort key to break ties and ensure determinism
      t -> (fullCrossScalaVersions.indexOf(taskCrossVersion), t.ctx.segments.render)
    }
    .toMap

  val sortedArtifacts = publishArtifacts.value.sortBy(groupedArtifacts)

  val boundaries  =
    for(x <- 0 to divisionCount)
    yield math.round((x.toDouble * sortedArtifacts.length) / divisionCount).toInt

  sortedArtifacts.slice(boundaries(shard-1), boundaries(shard))

}

def publishSonatype(publishArtifacts: mill.main.Tasks[PublishModule.PublishData],
                    shard: Int,
                    divisionCount: Int) =
  T.command{

    val x: Seq[(Seq[(os.Path, String)], Artifact)] = {
      mill.define.Target.sequence(partition(publishArtifacts, shard, divisionCount))().map{
        case PublishModule.PublishData(a, s) => (s.map{case (p, f) => (p.path, f)}, a)
      }
    }
    if (isMasterCommit)
      new SonatypePublisher(
        "https://oss.sonatype.org/service/local",
        "https://oss.sonatype.org/content/repositories/snapshots",
        sys.env("SONATYPE_DEPLOY_USER") + ":" + sys.env("SONATYPE_DEPLOY_PASSWORD"),
        true,
        Seq("--passphrase", sys.env("SONATYPE_PGP_PASSWORD"), "--no-tty", "--pinentry-mode", "loopback", "--batch", "--yes", "-a", "-b"),
        600000,
        600000,
        T.ctx().log,
        600000,
      ).publishAll(
        true,
        x:_*
      )
  }

