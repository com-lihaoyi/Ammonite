// plugins
import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`
import $ivy.`io.get-coursier::coursier-launcher:2.1.0-RC1`
import $file.ci.upload
// imports
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.{Await, ExecutionContext, Future, duration}
import scala.util.chaining.scalaUtilChainingOps
import coursier.mavenRepositoryString
import mill._
import mill.api.Result
import mill.contrib.bloop.Bloop
import mill.define.Command
import mill.main.Tasks
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalalib.api.ZincWorkerUtil._
import mill.testrunner.TestRunner

val ghOrg = "com-lihaoyi"
val ghRepo = "Ammonite"
val masterBranch = "main"
val publishBranches = Seq(masterBranch, "2.x")
val homePage = "https://ammonite.io"

val isMasterCommit =
  sys.env.get("GITHUB_REPOSITORY") == Some(s"${ghOrg}/${ghRepo}") &&
    sys.env.get("GITHUB_REF").exists(x => x.endsWith(s"/${masterBranch}"))

val isPublishableCommit =
  sys.env.get("GITHUB_REPOSITORY") == Some(s"${ghOrg}/${ghRepo}") &&
    sys.env.get("GITHUB_REF").exists(x =>
      publishBranches.exists(suffix => x.endsWith(s"/${suffix}"))
    )

val latestTaggedVersion = os.proc("git", "describe", "--abbrev=0", "--tags").call().out.trim

val gitHead = os.proc("git", "rev-parse", "HEAD").call().out.trim

val commitsSinceTaggedVersion = {
  os.proc("git", "rev-list", gitHead, "--not", latestTaggedVersion, "--count")
    .call()
    .out
    .trim
    .toInt
}

val scala2_12Versions = 9.to(19).map(v => s"2.12.${v}")
val scala2_13Versions = 2.to(13).map(v => s"2.13.${v}")
val scala32Versions = Seq("3.2.0", "3.2.1", "3.2.2")
val scala33Versions = Seq("3.3.0", "3.3.1", "3.3.2", "3.3.3")
val scala3Versions = scala32Versions ++ scala33Versions

val binCrossScalaVersions =
  Seq(scala2_12Versions.last, scala2_13Versions.last, scala32Versions.last)
val assemblyCrossScalaVersions =
  Seq(scala2_12Versions.last, scala2_13Versions.last, scala33Versions.last)
def isScala2_12_10OrLater(sv: String): Boolean = {
  (sv.startsWith("2.12.") && sv.stripPrefix("2.12.").length > 1) || sv.startsWith("2.13.")
}
val fullCrossScalaVersions = scala2_12Versions ++ scala2_13Versions ++ scala3Versions

val latestAssemblies = assemblyCrossScalaVersions.map(amm(_).assembly)

println("GITHUB REF " + sys.env.get("GITHUB_REF"))

val (buildVersion, unstable) = scala.util.Try(
  os.proc("git", "describe", "--exact-match", "--tags", "--always", gitHead)
    .call()
    .out
    .trim
).toOption match {
  case None =>
    val gitHash = os.proc("git", "rev-parse", "--short", "HEAD").call().out.trim
    (s"$latestTaggedVersion-$commitsSinceTaggedVersion-$gitHash", true)
  case Some(tagName) => (tagName, false)
}

val bspVersion = "2.1.0-M5"
val fastparseVersion = "3.0.2"
val scalametaVersion = "4.9.1"

object Deps {
  val acyclic = ivy"com.lihaoyi:::acyclic:0.3.11"
  val bsp4j = ivy"ch.epfl.scala:bsp4j:${bspVersion}"
  val bcprovJdk15on = ivy"org.bouncycastle:bcprov-jdk18on:1.77"
  val cask = ivy"com.lihaoyi::cask:0.9.1"
  val classPathUtil = ivy"io.get-coursier::class-path-util:0.1.4"
  val coursierInterface = ivy"io.get-coursier:interface:1.0.19"
  val coursierDependencyInterface = ivy"io.get-coursier::dependency-interface:0.2.3"
  val fansi = ivy"com.lihaoyi::fansi:0.4.0"
  val fastparse = ivy"com.lihaoyi::fastparse:$fastparseVersion"
  val geny = ivy"com.lihaoyi::geny:1.0.0"
  val javaparserCore = ivy"com.github.javaparser:javaparser-core:3.2.12"
  val javassist = ivy"org.javassist:javassist:3.21.0-GA"
  val jlineJna = ivy"org.jline:jline-terminal-jna:3.14.1"
  val jlineReader = ivy"org.jline:jline-reader:3.14.1"
  val jlineTerminal = ivy"org.jline:jline-terminal:3.14.1"
  val jsch = ivy"com.jcraft:jsch:0.1.55"
  val mainargs = ivy"com.lihaoyi::mainargs:0.5.4"
  val osLib = ivy"com.lihaoyi::os-lib:0.9.3"
  val pprint = ivy"com.lihaoyi::pprint:0.8.1"
  val requests = ivy"com.lihaoyi::requests:0.8.0"
  val scalacheck = ivy"org.scalacheck::scalacheck:1.17.0"
  val scalaCollectionCompat = ivy"org.scala-lang.modules::scala-collection-compat:2.11.0"
  def scalaCompiler(scalaVersion: String) = ivy"org.scala-lang:scala-compiler:${scalaVersion}"
  val scalaJava8Compat = ivy"org.scala-lang.modules::scala-java8-compat:1.0.2"
  val scalaparse = ivy"com.lihaoyi::scalaparse:$fastparseVersion"
  def scalaReflect(scalaVersion: String) = ivy"org.scala-lang:scala-reflect:${scalaVersion}"
  def scalaXml(sv: String) = {
    val ver =
      if (sv.startsWith("2.12.")) "1.3.0"
      else "2.0.1"
    ivy"org.scala-lang.modules::scala-xml:$ver"
  }
  val scalazCore = ivy"org.scalaz::scalaz-core:7.2.35"
  val semanticDbScalac = ivy"org.scalameta:::semanticdb-scalac:$scalametaVersion"
  val shapeless = ivy"com.chuusai::shapeless:2.3.3"
  val slf4jNop = ivy"org.slf4j:slf4j-nop:1.7.36"
  val sourcecode = ivy"com.lihaoyi::sourcecode:0.3.1"
  val sshdCore = ivy"org.apache.sshd:sshd-core:1.2.0"
  val scalametaCommon = ivy"org.scalameta::common:$scalametaVersion"
  val typename = ivy"org.tpolecat::typename:1.1.0"
  def upickle(sv: String) = {
    val ver =
      if (sv.startsWith("3.2.")) "3.1.0"
      else "3.1.3"
    ivy"com.lihaoyi::upickle:$ver"
  }
  val utest = ivy"com.lihaoyi::utest:0.8.2"
}

trait AmmInternalModule extends CrossSbtModule with Bloop.Module {
  // We need it to be a Boolean, not T[Boolean]
  def isCrossFullScalaVersion: Boolean = false
  def crossFullScalaVersion = T { isCrossFullScalaVersion }
  def skipBloop = {
    val versions =
      if (isCrossFullScalaVersion) assemblyCrossScalaVersions else binCrossScalaVersions
    // no need to expose the modules for old Scala versions support in Bloop / Metals
    !versions.contains(crossScalaVersion)
  }
  def artifactName = T {
    "ammonite-" + millOuterCtx.segments.parts.mkString("-").stripPrefix("amm-")
  }
  def isScala2 = T { scalaVersion().startsWith("2.") }
  def scalacOptions = T {
    if (isScala2()) Seq("-P:acyclic:force")
    else Nil
  }
  def compileIvyDeps = T {
    if (isScala2()) Agg(Deps.acyclic)
    else Agg[Dep]()
  }
  def scalacPluginIvyDeps = T {
    if (isScala2()) Agg(Deps.acyclic)
    else Agg[Dep]()
  }
  override def scalaLibraryIvyDeps = T {
    val scalaV = scalaVersion()
    val scalaO = scalaOrganization()
    if (isScala3(scalaV)) Agg(
      ivy"$scalaO::scala3-library:$scalaV"
    )
    else Agg(
      ivy"$scalaO:scala-library:$scalaV"
    )
  }
  trait AmmTests extends super.Tests with TestModule.Utest {
    def ivyDeps = super.ivyDeps() ++ Agg(Deps.utest)
    def forkArgs = Seq("-Xmx2g", "-Dfile.encoding=UTF8")
  }
  def allIvyDeps = T { transitiveIvyDeps() ++ scalaLibraryIvyDeps() }
  def sources = T.sources {
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
          if (
            sv.startsWith("2.13.") && sv.stripPrefix("2.13.").toInt >= 1 && sv.stripPrefix(
              "2.13."
            ).toInt <= 11
          )
            millSourcePath / "src" / "main" / "scala-2.13.1-2.13.11"
          else if (sv.startsWith("2.13.") && sv.stripPrefix("2.13.").toInt >= 12)
            millSourcePath / "src" / "main" / "scala-2.13.12+"
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
      if (sv.startsWith("3.3") && sv.stripPrefix("3.3.").toInt >= 2)
        Seq(PathRef(millSourcePath / "src" / "main" / "scala-3.3.2+"))
      else if (sv.startsWith("3"))
        Seq(PathRef(millSourcePath / "src" / "main" / "scala-3.0.0-3.3.1"))
      else Nil

    super.sources() ++ extraDir ++ extraDir2 ++ extraDir3 ++ extraDir4 ++ extraDir5
  }
  def externalSources = T {
    resolveDeps(allIvyDeps, sources = true)()
  }
  def repositories = super.repositories ++ Seq(
    mvn"https://scala-ci.typesafe.com/artifactory/scala-integration"
  )
  override implicit def crossSbtModuleResolver: mill.define.Cross.Resolver[CrossModuleBase] =
    new mill.define.Cross.Resolver[CrossModuleBase] {
      def resolve[V <: CrossModuleBase](c: Cross[V]): V = {
        crossScalaVersion
          .split('.')
          .inits
          .filter { v =>
            if (isScala3(crossScalaVersion)) v.length != 2 else v.length != 1
          }
          .flatMap(prefix =>
            c.items
              .map(_._2)
              .find(_.crossScalaVersion.split('.').startsWith(prefix))
          )
          .collectFirst { case x => x }
          .getOrElse {
            throw new Exception(
              s"Unable to find compatible cross version between $crossScalaVersion and " +
                c.items.map(_._2.crossScalaVersion).mkString(",")
            )
          }

      }
    }
}
trait AmmModule extends AmmInternalModule with PublishModule {
  def publishVersion = buildVersion
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "com.lihaoyi",
    url = s"https://github.com/${ghOrg}/${ghRepo}",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github(ghOrg, ghRepo),
    developers = Seq(
      Developer("lihaoyi", "Li Haoyi", "https://github.com/lihaoyi")
    )
  )

  def transitiveJars: T[Agg[PathRef]] = T {
    mill.define.Target.traverse(this +: moduleDeps)(m =>
      T.task { m.jar() }
    )()
  }

  def transitiveSourceJars: T[Agg[PathRef]] = T {
    mill.define.Target.traverse(this +: moduleDeps)(m =>
      T.task { m.sourceJar() }
    )()
  }

  override def javacOptions = Seq("-source", "1.8", "-target", "1.8")
}
trait AmmDependenciesResourceFileModule extends JavaModule {
  def dependencyResourceFileName: String
  def dependencyFileResources = T {
    val deps0 = T.task { compileIvyDeps() ++ transitiveIvyDeps() }()
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

object terminal extends Cross[TerminalModule](binCrossScalaVersions: _*)
class TerminalModule(val crossScalaVersion: String) extends AmmModule {
  def ivyDeps = T {
    super.ivyDeps() ++ Agg(
      Deps.fansi,
      Deps.sourcecode
    )
  }
  object test extends AmmTests {
    def ivyDeps = super.ivyDeps() ++ Agg(Deps.sourcecode)
  }
}

object amm extends Cross[MainModule](fullCrossScalaVersions: _*) {
  object util extends Cross[UtilModule](binCrossScalaVersions: _*)
  class UtilModule(val crossScalaVersion: String) extends AmmModule {
    def moduleDeps = Seq()
    def ivyDeps = T {
      super.ivyDeps() ++ Agg(
        Deps.osLib,
        Deps.typename,
        Deps.scalaCollectionCompat,
        Deps.fansi,
        Deps.pprint
      )
    }
    def compileIvyDeps = super.compileIvyDeps() ++
      (if (isScala3(crossScalaVersion)) Agg.empty[Dep] else Agg(Deps.scalaReflect(scalaVersion())))
  }

  object runtime extends Cross[RuntimeModule](fullCrossScalaVersions: _*)
  class RuntimeModule(val crossScalaVersion: String) extends AmmModule {
    def moduleDeps = Seq(amm.util(), interp.api(), amm.repl.api())
    def isCrossFullScalaVersion = true
    def ivyDeps = super.ivyDeps() ++ Agg(
      Deps.classPathUtil,
      Deps.upickle(crossScalaVersion),
      Deps.requests,
      Deps.mainargs,
      Deps.coursierDependencyInterface
    )
  }

  object compiler extends Cross[CompilerModule](fullCrossScalaVersions: _*) {
    object interface extends Cross[CompilerInterfaceModule](fullCrossScalaVersions: _*)
    class CompilerInterfaceModule(val crossScalaVersion: String) extends AmmModule {
      def isCrossFullScalaVersion = true
      def moduleDeps = Seq(amm.util())
      def exposedClassPath = T {
        runClasspath() ++
          externalSources() ++
          transitiveJars() ++
          transitiveSourceJars()
      }
    }
  }
  class CompilerModule(val crossScalaVersion: String) extends AmmModule {
    def supports3 = true
    def moduleDeps = Seq(amm.compiler.interface(), amm.util(), amm.repl.api())
    def isCrossFullScalaVersion = true
    def ivyDeps = T {
      val scalaSpecificDeps =
        if (isScala2())
          Agg(
            Deps.scalaCompiler(scalaVersion()),
            Deps.scalaparse,
            Deps.scalaXml(scalaVersion())
          )
        else
          Agg[Dep](
            ivy"org.scala-lang::scala3-compiler:${scalaVersion()}",
            ivy"org.ow2.asm:asm:9.6"
          )
      super.ivyDeps() ++ scalaSpecificDeps ++ Agg(
        Deps.javassist,
        Deps.javaparserCore
      )
    }

    def exposedClassPath = T {
      runClasspath() ++
        externalSources() ++
        transitiveJars() ++
        transitiveSourceJars()
    }

    object test extends AmmTests
  }

  object interp extends Cross[InterpModule](fullCrossScalaVersions: _*) {
    object api extends Cross[InterpApiModule](fullCrossScalaVersions: _*)
    class InterpApiModule(val crossScalaVersion: String) extends AmmModule
        with AmmDependenciesResourceFileModule {
      def moduleDeps = Seq(amm.compiler.interface(), amm.util())
      def isCrossFullScalaVersion = true
      def dependencyResourceFileName = "amm-interp-api-dependencies.txt"
      def ivyDeps = super.ivyDeps() ++ Agg(
        Deps.coursierInterface
      )
      override def docJar = if (isScala3(crossScalaVersion)) T {
        val outDir = T.ctx().dest
        val javadocDir = outDir / "javadoc"
        os.makeDir.all(javadocDir)
        mill.api.Result.Success(mill.modules.Jvm.createJar(Agg(javadocDir))(outDir))
      }
      else super.docJar
      def constantsSourceDir = T {
        val dir = T.dest / "src"
        val dest = dir / "Constants.scala"
        val code =
          s"""package ammonite.interp.script
             |
             |/** Build-time constants. Generated by mill. */
             |object Constants {
             |  def semanticDbVersion = "${Deps.semanticDbScalac.dep.version}"
             |}
             |""".stripMargin
        os.write(dest, code, createFolders = true)
        PathRef(dir)
      }
      override def generatedSources: T[Seq[PathRef]] =
        super.generatedSources() ++ Seq(constantsSourceDir())
    }
  }
  class InterpModule(val crossScalaVersion: String) extends AmmModule {
    def moduleDeps = Seq(amm.util(), amm.runtime(), amm.compiler.interface())
    def isCrossFullScalaVersion = true
    def ivyDeps = super.ivyDeps() ++ Agg(
      Deps.bsp4j,
      Deps.fastparse
    ) ++ Agg(
      Deps.scalametaCommon
    ).map(dep =>
      if (isScala3(crossScalaVersion))
        dep.withDottyCompat(crossScalaVersion)
          // we remove transitive _2.13 dependencies from Scala 3 and
          // then we add it back with _3
          .exclude("com.lihaoyi" -> "sourcecode_2.13")
      else dep
    ) ++ (if (isScala3(crossScalaVersion)) Agg(Deps.sourcecode) else Agg.empty[Dep])
  }

//  object `test-runner` extends mill.scalalib.SbtModule {
//    def scalaVersion = "2.12.8"
//    def ivyDeps = super.ivyDeps() ++ Agg(
//      ivy"com.lihaoyi::mill-scalalib:${sys.props("MILL_VERSION")}"
//    )
//  }

  object repl extends Cross[ReplModule](fullCrossScalaVersions: _*) {

    object api extends Cross[ReplApiModule](fullCrossScalaVersions: _*)
    class ReplApiModule(val crossScalaVersion: String) extends AmmModule
        with AmmDependenciesResourceFileModule {
      def isCrossFullScalaVersion = true
      def dependencyResourceFileName = "amm-dependencies.txt"
      def moduleDeps = Seq(amm.util(), interp.api())
      def ivyDeps = super.ivyDeps() ++ Agg(
        Deps.mainargs,
        Deps.geny
      )
      def compileIvyDeps = super.compileIvyDeps() ++ (if (isScala3(crossScalaVersion))
                                                        Agg.empty[Dep]
                                                      else Agg(Deps.scalaReflect(scalaVersion())))

      def generatedSources = T {
        Seq(PathRef(generateConstantsFile(buildVersion, bspVersion = bspVersion)))
      }

      def exposedClassPath = T {
        amm.repl.api().runClasspath() ++
          amm.repl.api().externalSources() ++
          amm.repl.api().transitiveJars() ++
          amm.repl.api().transitiveSourceJars()
      }
    }

  }
  class ReplModule(val crossScalaVersion: String) extends AmmModule {
    def isCrossFullScalaVersion = true
    def moduleDeps = Seq(
      amm.util(),
      amm.runtime(),
      amm.interp(),
      terminal(),
      amm.compiler.interface()
    )
    def ivyDeps = super.ivyDeps() ++ Agg(
      Deps.jlineTerminal,
      Deps.jlineJna,
      Deps.jlineReader,
      Deps.scalaXml(scalaVersion())
    )
    def compileIvyDeps = super.compileIvyDeps() ++ (if (isScala3(crossScalaVersion)) Agg.empty[Dep]
                                                    else Agg(Deps.scalaReflect(scalaVersion())))

    object test extends AmmTests with AmmDependenciesResourceFileModule {
      def crossScalaVersion = ReplModule.this.crossScalaVersion
      def scalaVersion = ReplModule.this.scalaVersion
      def dependencyResourceFileName = "amm-test-dependencies.txt"
      def moduleDeps = super.moduleDeps ++ Seq(amm.compiler())

      def thinWhitelist = T {
        generateApiWhitelist(
          amm.repl.api().exposedClassPath() ++
            amm.compiler().exposedClassPath() ++
            Seq(compile().classes) ++
            resolveDeps(T.task { compileIvyDeps() ++ transitiveIvyDeps() })()
        )
      }

      def localClasspath = T {
        super.localClasspath() ++ Agg(thinWhitelist())
      }

      def resources = T.sources {
        (super.resources() ++
          ReplModule.this.sources() ++
          ReplModule.this.externalSources() ++
          resolveDeps(ivyDeps, sources = true)()).distinct
      }
      def ivyDeps = super.ivyDeps() ++ amm.compiler().ivyDeps() ++ Agg(
        Deps.scalazCore
      )
    }
  }
}

class MainModule(val crossScalaVersion: String) extends AmmModule {

  def isCrossFullScalaVersion = true

  def mainClass = Some("ammonite.AmmoniteMain")

  def artifactName = "ammonite"

  def moduleDeps = Seq(
    terminal(),
    amm.util(),
    amm.runtime(),
    amm.interp.api(),
    amm.repl.api(),
    amm.interp(),
    amm.repl(),
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

  def prependShellScript = T {
    mill.modules.Jvm.launcherUniversalScript(
      mainClass().get,
      Agg("$0"),
      Agg("%~dpnx0"),
      // G1 Garbage Collector is awesome https://github.com/lihaoyi/Ammonite/issues/216
      Seq("-Xmx500m", "-XX:+UseG1GC")
    )
  }

  def thinWhitelist = T {
    generateApiWhitelist(
      amm.repl.api().exposedClassPath() ++
        amm.compiler().exposedClassPath()
    )
  }
  def localClasspath = T {
    super.localClasspath() ++ Agg(thinWhitelist())
  }

  def launcher = {
    val isWindows = scala.util.Properties.isWin
    if (isWindows)
      T {
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
      T {
        super.launcher()
      }
  }

  object test extends AmmTests {
    def moduleDeps = super.moduleDeps ++ Seq(amm.compiler().test, amm.repl().test)
    def ivyDeps = super.ivyDeps() ++ Agg(
      Deps.scalaJava8Compat
    )

    def thinWhitelist = T {
      generateApiWhitelist(
        amm.repl.api().exposedClassPath() ++
          amm.compiler().exposedClassPath() ++
          Seq(amm.repl().test.compile().classes, compile().classes) ++
          resolveDeps(T.task { compileIvyDeps() ++ transitiveIvyDeps() })()
      )
    }

    def localClasspath = T {
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

  val thinClasspathEntries = replApiCp.map(_.path).flatMap { cpRoot =>
    if (os.isFile(cpRoot) && cpRoot.ext == "jar") {
      val zip = new java.util.zip.ZipFile(cpRoot.toIO)
      import collection.JavaConverters._
      for (e <- zip.entries().asScala) yield e.getName
    } else if (os.isDir(cpRoot)) {
      for (sub <- os.walk(cpRoot)) yield sub.relativeTo(cpRoot).toString
    } else if (!os.exists(cpRoot)) Nil
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

object integration extends Cross[IntegrationModule](fullCrossScalaVersions: _*)
class IntegrationModule(val crossScalaVersion: String) extends AmmInternalModule {
  def moduleDeps = Seq(amm())
  def ivyDeps = T {
    super.ivyDeps() ++ (
      if (scalaVersion().startsWith("2.13."))
        Agg(Deps.cask)
      else
        Agg.empty
    )
  }
  object test extends AmmTests {
    def testLauncher = T {
      if (scala.util.Properties.isWin)
        amm().launcher().path.toString
      else
        amm().assembly().path.toString
    }
    def forkEnv = super.forkEnv() ++ Seq(
      "AMMONITE_ASSEMBLY" -> testLauncher()
    )
  }
}

object sshd extends Cross[SshdModule](fullCrossScalaVersions: _*)
class SshdModule(val crossScalaVersion: String) extends AmmModule {
  def moduleDeps = Seq(amm())
  def isCrossFullScalaVersion = true
  def ivyDeps = super.ivyDeps() ++ Agg(
    // sshd-core 1.3.0 requires java8
    Deps.sshdCore,
    Deps.bcprovJdk15on
  )
  object test extends AmmTests {
    def ivyDeps = super.ivyDeps() ++ Agg(
      // slf4j-nop makes sshd server use logger that writes into the void
      Deps.slf4jNop,
      Deps.jsch,
      Deps.scalacheck
    )
  }
}

/**
 * Selects all cross module instances, that match the given predicate.
 * In Mill 0.11, this can be hopefully replaced with a simple filter on the `crossValue`.
 */
def selectCrossPrefix[T <: Module, V](
    crossModule: Cross[T],
    predicate: String => Boolean
)(accessor: T => V): Seq[V] =
  crossModule.items.collect {
    case (List(key: String), mod) if predicate(key) => accessor(mod)
  }
    .tap { mods =>
      if (mods.isEmpty) sys.error(s"No matching cross-instances found in ${crossModule}")
    }

def unitTest(scalaBinaryVersion: String = ""): Command[Seq[(String, Seq[TestRunner.Result])]] = {
  val pred = (_: String).startsWith(scalaBinaryVersion)
  val tests = Seq(
    selectCrossPrefix(terminal, pred)(_.test),
    selectCrossPrefix(amm.repl, pred)(_.test),
    selectCrossPrefix(amm, pred)(_.test),
    selectCrossPrefix(sshd, pred)(_.test)
  ).flatten

  val log = T.task { T.log.outputStream.println(s"Testing modules: ${tests.mkString(", ")}") }

  T.command {
    log()
    T.traverse(tests)(_.testCached)()
  }
}

def integrationTest(scalaVersion: String = "") = T.command {
  T.traverse(
    selectCrossPrefix(integration, _.startsWith(scalaVersion))(_.test)
  )(_.testCached)()
}

def generateConstantsFile(
    version: String = buildVersion,
    unstableVersion: String = "<fill-me-in-in-Constants.scala>",
    bspVersion: String = "<fill-me-in-in-Constants.scala>",
    curlUrl: String = "<fill-me-in-in-Constants.scala>",
    unstableCurlUrl: String = "<fill-me-in-in-Constants.scala>",
    oldCurlUrls: Seq[(String, String)] = Nil,
    oldUnstableCurlUrls: Seq[(String, String)] = Nil,
    returnDirectory: Boolean = true
)(implicit ctx: mill.util.Ctx.Dest) = {
  val versionTxt = s"""
    package ammonite
    object Constants{
      val version = "$version"
      val unstableVersion = "$unstableVersion"
      val bspVersion = "$bspVersion"
      val curlUrl = "$curlUrl"
      val unstableCurlUrl = "$unstableCurlUrl"
      val oldCurlUrls = Seq[(String, String)](
        ${oldCurlUrls.map { case (name, value) => s""" "$name" -> "$value" """ }.mkString(",\n")}
      )
      val oldUnstableCurlUrls = Seq[(String, String)](
        ${oldUnstableCurlUrls.map { case (name, value) => s""" "$name" -> "$value" """ }.mkString(
      ",\n"
    )}
      )
    }
  """
  println("Writing Constants.scala")

  val dir = ctx.dest / "src"
  os.write(dir / "Constants.scala", versionTxt, createFolders = true)
  if (returnDirectory) dir
  else dir / "Constants.scala"
}

def generateDependenciesFile(fileName: String, deps: Seq[coursier.Dependency])(implicit
    ctx: mill.util.Ctx.Dest
) = {

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
  if (!isPublishableCommit) T.command {
    println("MISC COMMIT: generating executable but not publishing")
    T.sequence(latestAssemblies)()
  }
  else T.command {
    val latestAssemblyJars = T.sequence(latestAssemblies)()

    println("MASTER COMMIT: Creating a release")
    if (!unstable) {
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

    for ((version, jar) <- assemblyCrossScalaVersions.zip(latestAssemblyJars)) {
      println("MASTER COMMIT: Publishing Executable for Scala " + version)
      // Prepare executable

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

def publishDocs(skipDeploy: Boolean = false): Command[Unit] = {
  val ammoniteAssembly = amm(scala2_13Versions.last).assembly
  // Disable doc auto-publishing for now, as the recent modularization means we
  // need to make significant changes to the readme and that'll time.
  if (!isMasterCommit) T.command {
    println("MISC COMMIT: Building readme for verification")
    try {
      os.proc(
        "sbt",
        "readme/run"
      ).call(
        env = Map(
          "AMMONITE_ASSEMBLY" -> ammoniteAssembly().path.toString,
          "CONSTANTS_FILE" -> generateConstantsFile(returnDirectory = false).toString
        )
      )
    } catch {
      case e =>
        println(e)
        e.printStackTrace()
        throw e
    }
    ()
  }
  else T.command {
    println("MASTER COMMIT: Updating version and publishing to Github Pages")

    if (!skipDeploy) {
      val deployKey = sys.env("DEPLOY_KEY").replace("\\n", "\n")
      os.write(os.pwd / "deploy_key", deployKey)
    }

    val (stableKey, unstableKey, oldStableKeys, oldUnstableKeys) =
      if (!unstable) {
        (
          s"$latestTaggedVersion/2.13-$latestTaggedVersion",
          s"$latestTaggedVersion/2.13-$latestTaggedVersion",
          for (v <- Seq("2.12"))
            yield s"$latestTaggedVersion/$v-$latestTaggedVersion",
          for (v <- Seq("2.12"))
            yield s"$latestTaggedVersion/$v-$latestTaggedVersion"
        )
      } else {
        (
          s"$latestTaggedVersion/2.13-$latestTaggedVersion",
          s"$latestTaggedVersion/2.13-$buildVersion",
          for (v <- Seq("2.12"))
            yield s"$latestTaggedVersion/$v-$latestTaggedVersion",
          for (v <- Seq("2.12"))
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
      for (k <- oldStableKeys)
        yield (k, s"https://github.com/${ghOrg}/${ghRepo}/releases/download/$k"),
      for (k <- oldUnstableKeys)
        yield (k, s"https://github.com/${ghOrg}/${ghRepo}/releases/download/$k"),
      returnDirectory = false
    )

    os.proc(
      "sbt",
      "readme/run"
    ).call(
      env = Map(
        "AMMONITE_ASSEMBLY" -> ammoniteAssembly().path.toString,
        "CONSTANTS_FILE" -> constantsFile.toString
      )
    )
    if (skipDeploy) {
      println("Skip deployment")
    } else {
      println("Deploying ...")
      os.proc("ci/deploy_master_docs.sh").call()
    }
    ()
  }
}

def partition(
    publishArtifacts: mill.main.Tasks[PublishModule.PublishData],
    shard: Int,
    divisionCount: Int
) = {

  val groupedArtifacts = publishArtifacts.value
    .map { t =>
      val taskCrossVersion = t.ctx.segments.value
        .collectFirst { case mill.define.Segment.Cross(List(v)) => v }
        .get

      // Sort primarily on the scalaVersion, using the rendered name of the
      // task as the secondary sort key to break ties and ensure determinism
      t -> (fullCrossScalaVersions.indexOf(taskCrossVersion), t.ctx.segments.render)
    }
    .toMap

  val sortedArtifacts = publishArtifacts.value.sortBy(groupedArtifacts)

  val boundaries =
    for (x <- 0 to divisionCount)
      yield math.round((x.toDouble * sortedArtifacts.length) / divisionCount).toInt

  sortedArtifacts.slice(boundaries(shard - 1), boundaries(shard))

}

def publishSonatype(
    publishArtifacts: mill.main.Tasks[PublishModule.PublishData],
    shard: Int,
    divisionCount: Int
) =
  T.command {

    val x: Seq[(Seq[(os.Path, String)], Artifact)] = {
      mill.define.Target.sequence(partition(publishArtifacts, shard, divisionCount))().map {
        case PublishModule.PublishData(a, s) => (s.map { case (p, f) => (p.path, f) }, a)
      }
    }
    if (isPublishableCommit)
      new SonatypePublisher(
        uri = "https://oss.sonatype.org/service/local",
        snapshotUri = "https://oss.sonatype.org/content/repositories/snapshots",
        credentials = sys.env("SONATYPE_DEPLOY_USER") + ":" + sys.env("SONATYPE_DEPLOY_PASSWORD"),
        signed = true,
        gpgArgs = Seq(
          "--passphrase",
          sys.env("SONATYPE_PGP_PASSWORD"),
          "--no-tty",
          "--pinentry-mode",
          "loopback",
          "--batch",
          "--yes",
          "-a",
          "-b"
        ),
        readTimeout = 600000,
        connectTimeout = 600000,
        log = T.ctx().log,
        workspace = T.workspace,
        env = T.env,
        awaitTimeout = 600000,
        stagingRelease = true
      ).publishAll(
        true,
        x: _*
      )
  }

/**
 * Somethime, the Mill publish command fails although the Sonatype publishing went through.
 * This command checks, whether all artifacts are publshed.
 * Run with:
 * {{{
 * mill checkPublishedArtifacts --artifacts __.publishSelfDependency --version {version} --ttl "1 sec"
 * }}}
 * See also https://github.com/com-lihaoyi/Ammonite/pull/1453
 */
def checkPublishedArtifacts(artifacts: Tasks[Artifact], version: String, ttl: String = "1 hour") =
  T.command {
    val coords = T.sequence(artifacts.value)()
    val next = new AtomicInteger(0)
    val fut = coords.map { coord =>
      Future {
        val dep = s"${coord.group}:${coord.id}:${version}"
        println(s"[${next.incrementAndGet()}/${coords.size}] Checking ${dep}")
        val res = os.proc("cs", "complete-dep", dep, "-l", ttl)
          .call().out.text().trim()
        // println(res)
        Option.when(!res.contains(version))(dep)
      }(ExecutionContext.global)
    }
    val missing = fut.map(Await.result(_, duration.Duration.Inf)).flatten
    if (missing.isEmpty) {
      Result.Success(s"All artifacts published for version ${version}")
    } else {
      val msg =
        if (missing.size == coords.size) s"All artifacts missing for version ${version}"
        else s"Missing ${missing.size} of ${coords.size} published artifacts: ${
            missing.mkString("\n- ", "\n- ", "")
          }"
      Result.Failure(msg)
    }
  }
