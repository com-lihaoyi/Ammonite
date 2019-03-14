import mill._, scalalib._, publish._
import ammonite.ops._, ImplicitWd._
import $file.ci.upload

import $ivy.`io.get-coursier::coursier-bootstrap:2.0.0-RC2-4+5-6ccc572b`

val isMasterCommit =
  sys.env.get("TRAVIS_PULL_REQUEST") == Some("false") &&
  (sys.env.get("TRAVIS_BRANCH") == Some("master") || sys.env("TRAVIS_TAG") != "")

val latestTaggedVersion = %%('git, 'describe, "--abbrev=0", "--tags").out.trim

val commitsSinceTaggedVersion = {
  %%('git, "rev-list", 'master, "--not", latestTaggedVersion, "--count").out.trim.toInt
}


val binCrossScalaVersions = Seq("2.12.8", "2.13.0")
val fullCrossScalaVersions = Seq(
  "2.12.0", "2.12.1", "2.12.2", "2.12.3", "2.12.4", "2.12.6", "2.12.7", "2.12.8",
  "2.13.0"
)

val latestAssemblies = binCrossScalaVersions.map(amm(_).bootstrap)

val (buildVersion, unstable) = sys.env.get("TRAVIS_TAG") match{
  case Some(v) if v != "" => (v, false)
  case _ =>
    val gitHash = %%("git", "rev-parse", "--short", "HEAD").out.trim
    (s"$latestTaggedVersion-$commitsSinceTaggedVersion-${gitHash}", true)
}

trait AmmInternalModule extends mill.scalalib.CrossSbtModule{
  def artifactName = "ammonite-" + millOuterCtx.segments.parts.last
  def testFramework = "utest.runner.Framework"
  def scalacOptions = Seq("-P:acyclic:force", "-target:jvm-1.7")
  def compileIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.2.0")
  def scalacPluginIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.2.0")
  trait Tests extends super.Tests{
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.1")
    def testFrameworks = Seq("utest.runner.Framework")
    def forkArgs = Seq("-XX:MaxPermSize=2g", "-Xmx4g", "-Dfile.encoding=UTF8")
    def sources = T.sources{
      val is211 = crossScalaVersion.startsWith("2.11.")
      val is212 = crossScalaVersion.startsWith("2.12.")
      val shared211_212 =
        if (is211 || is212)
          Seq(PathRef(millSourcePath / 'src / 'test / "scala-2.11_2.12"))
        else
          Seq()
      super.sources() ++
        shared211_212
    }
  }
  def allIvyDeps = T{transitiveIvyDeps() ++ scalaLibraryIvyDeps()}
  def externalSources = T{
    resolveDeps(allIvyDeps, sources = true)()
  }
  def sources = T.sources{
    val is211 = crossScalaVersion.startsWith("2.11.")
    val is212 = crossScalaVersion.startsWith("2.12.")
    val is213 = crossScalaVersion.startsWith("2.13.")
    val shared211_212 =
      if (is211 || is212)
        Seq(PathRef(millSourcePath / 'src / 'main / "scala-2.11_2.12"))
      else
        Seq()
    val shared212_213 =
      if (is212 || is213)
        Seq(PathRef(millSourcePath / 'src / 'main / "scala-2.12_2.13"))
      else
        Seq()
    super.sources() ++
      shared211_212 ++
      shared212_213
  }
}
trait AmmModule extends AmmInternalModule with PublishModule{
  def publishVersion = buildVersion
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "com.lihaoyi",
    url = "https://github.com/lihaoyi/Ammonite",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("lihaoyi", "ammonite"),
    developers = Seq(
      Developer("lihaoyi", "Li Haoyi","https://github.com/lihaoyi")
    )
  )

  trait CustomRunnerTests extends super.Tests {
    def forkArgs = T {
      val testBaseCp = amm.`test-api`().runClasspath() ++
        amm.`test-api`().sources() ++
        amm.`test-api`().transitiveSources() ++
        amm.`test-api`().externalSources()
      super.forkArgs() ++ Seq(
        "-Dtest.base.classpath=" +
          testBaseCp
            .map(_.path.toIO.getAbsolutePath)
            .mkString(java.io.File.pathSeparator)
      )
    }

    def test(args: String*) = T.command{
      val outputPath = T.ctx().dest/"out.json"

      mill.modules.Jvm.runSubprocess(
        mainClass = "ammonite.TestRunner",
        classPath = amm.`test-runner`.runClasspath().map(_.path),
        jvmArgs = forkArgs(),
        envArgs = forkEnv(),
        mainArgs =
          Seq(testFrameworks().length.toString) ++
            testFrameworks() ++
            Seq(runClasspath().length.toString) ++
            runClasspath().map(_.path.toString) ++
            Seq(args.length.toString) ++
            args ++
            Seq(outputPath.toString, T.ctx().log.colored.toString, compile().classes.path.toString, T.ctx().home.toString),
        workingDir = forkWorkingDir()
      )

      try {
        val jsonOutput = ujson.read(outputPath.toIO)
        val (doneMsg, results) = upickle.default.read[(String, Seq[TestRunner.Result])](jsonOutput)
        TestModule.handleResults(doneMsg, results)
      } catch {
        case e: Throwable =>
          mill.eval.Result.Failure("Test reporting failed: " + e)
      }
    }
  }

  def transitiveSources: T[Seq[PathRef]] = T{
    mill.define.Task.traverse(this +: moduleDeps)(m =>
      T.task{m.sources()}
    )().flatten
  }

  def transitiveJars: T[Agg[PathRef]] = T{
    mill.define.Task.traverse(this +: moduleDeps)(m =>
      T.task{m.jar()}
    )()
  }

  def transitiveSourceJars: T[Agg[PathRef]] = T{
    mill.define.Task.traverse(this +: moduleDeps)(m =>
      T.task{m.sourceJar()}
    )()
  }

}
trait AmmDependenciesResourceFileModule extends JavaModule{
  def crossScalaVersion: String
  def dependencyResourceFileName: String
  def resources = T.sources {

    val deps0 = T.task{compileIvyDeps() ++ transitiveIvyDeps()}()
    val (_, res) = mill.modules.Jvm.resolveDependenciesMetadata(
      repositories,
      deps0.map(resolveCoursierDependency().apply(_)),
      deps0.filter(_.force).map(resolveCoursierDependency().apply(_)),
      mapDependencies = Some(mapDependencies())
    )

    super.resources() ++
    Seq(PathRef(generateDependenciesFile(
      crossScalaVersion,
      dependencyResourceFileName,
      res.minDependencies.toSeq
    )))
  }
}

object ops extends Cross[OpsModule](binCrossScalaVersions:_*)
class OpsModule(val crossScalaVersion: String) extends AmmModule{
  def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.3.0",
    ivy"org.scala-lang.modules::scala-collection-compat:2.0.0"
  )
  def scalacOptions = super.scalacOptions().filter(!_.contains("acyclic"))
  object test extends Tests
}

object terminal extends Cross[TerminalModule](binCrossScalaVersions:_*)
class TerminalModule(val crossScalaVersion: String) extends AmmModule{
  def ivyDeps = Agg(
    ivy"com.lihaoyi::sourcecode:0.1.7",
    ivy"com.lihaoyi::fansi:0.2.7"
  )
  def compileIvyDeps = Agg(
    ivy"org.scala-lang:scala-reflect:$crossScalaVersion"
  )

  object test extends Tests
}

object amm extends Cross[MainModule](fullCrossScalaVersions:_*){
  object util extends Cross[UtilModule](binCrossScalaVersions:_*)
  class UtilModule(val crossScalaVersion: String) extends AmmModule{
    def moduleDeps = Seq(ops())
    def ivyDeps = Agg(
      ivy"com.lihaoyi::pprint:0.5.5",
      ivy"com.lihaoyi::fansi:0.2.7",
    )
    def compileIvyDeps = Agg(
      ivy"org.scala-lang:scala-reflect:$crossScalaVersion"
    )

  }


  object `interp-api` extends Cross[InterpApiModule](fullCrossScalaVersions:_*)
  class InterpApiModule(val crossScalaVersion: String) extends AmmModule{
    def moduleDeps = Seq(ops(), amm.util())
    def crossFullScalaVersion = true
    def ivyDeps = Agg(
      ivy"org.scala-lang:scala-compiler:$crossScalaVersion",
      ivy"org.scala-lang:scala-reflect:$crossScalaVersion",
      ivy"io.get-coursier::coursier:2.0.0-RC2"
    )
  }

  object `repl-api` extends Cross[ReplApiModule](fullCrossScalaVersions:_*)
  class ReplApiModule(val crossScalaVersion: String) extends AmmModule with AmmDependenciesResourceFileModule{
    def crossFullScalaVersion = true
    def dependencyResourceFileName = "amm-dependencies.txt"
    def moduleDeps = Seq(
      ops(), amm.util(),
      `interp-api`()
    )
    def ivyDeps = Agg(
      ivy"com.github.scopt::scopt:3.7.1"
    )

    def generatedSources = T{
      Seq(PathRef(generateConstantsFile(buildVersion)))
    }
  }

  object `test-api` extends Cross[TestApiModule](fullCrossScalaVersions:_*)
  class TestApiModule(val crossScalaVersion: String) extends AmmModule with AmmDependenciesResourceFileModule{
    def crossFullScalaVersion = true
    def dependencyResourceFileName = "amm-test-dependencies.txt"
    def moduleDeps = Seq(
      `repl-api`()
    )
  }


  object runtime extends Cross[RuntimeModule](binCrossScalaVersions:_*)
  class RuntimeModule(val crossScalaVersion: String) extends AmmModule{
    def moduleDeps = Seq(ops(), amm.util(), `interp-api`(), `repl-api`())
    def ivyDeps = Agg(
      ivy"com.lihaoyi::upickle:0.7.5"
    )
  }

  object interp extends Cross[InterpModule](fullCrossScalaVersions:_*)
  class InterpModule(val crossScalaVersion: String) extends AmmModule{
    def moduleDeps = Seq(ops(), amm.util(), amm.runtime())
    def crossFullScalaVersion = true
    def ivyDeps = Agg(
      ivy"org.scala-lang:scala-compiler:$crossScalaVersion",
      ivy"org.scala-lang:scala-reflect:$crossScalaVersion",
      ivy"com.lihaoyi::scalaparse:2.1.3",
      ivy"org.javassist:javassist:3.21.0-GA"
    )
  }

  object `test-runner` extends mill.scalalib.SbtModule {
    def scalaVersion = "2.12.8"
    def ivyDeps = Agg(
      ivy"com.lihaoyi::mill-scalalib:${sys.props("MILL_VERSION")}"
    )
  }

  object repl extends Cross[ReplModule](fullCrossScalaVersions:_*)
  class ReplModule(val crossScalaVersion: String) extends AmmModule{
    def crossFullScalaVersion = true
    def moduleDeps = Seq(
      ops(), amm.util(),
      amm.runtime(), amm.interp(),
      terminal()
    )
    def ivyDeps = Agg(
      ivy"org.jline:jline-terminal:3.6.2",
      ivy"org.jline:jline-terminal-jna:3.6.2",
      ivy"org.jline:jline-reader:3.6.2",
      ivy"com.github.javaparser:javaparser-core:3.2.5",
      ivy"com.github.scopt::scopt:3.7.1"
    )

    object test extends CustomRunnerTests{
      def moduleDeps = super.moduleDeps ++ Seq(
        amm.`test-api`()
      )
      def crossScalaVersion = ReplModule.this.crossScalaVersion
      def resources = T.sources {
        (super.resources() ++
        ReplModule.this.sources() ++
        ReplModule.this.externalSources() ++
        resolveDeps(ivyDeps, sources = true)()).distinct
      }
      def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.scalaz::scalaz-core:7.2.27"
      )
    }
  }
}
class MainModule(val crossScalaVersion: String) extends AmmModule with AmmDependenciesResourceFileModule{

  def artifactName = "ammonite"

  def crossFullScalaVersion = true

  def mainClass = Some("ammonite.Main")

  def moduleDeps = Seq(
    terminal(), ops(),
    amm.util(), amm.runtime(),
    amm.`interp-api`(),
    amm.`repl-api`(),
    amm.interp(), amm.repl()
  )

  def runClasspath =
    super.runClasspath() ++
    ops().sources() ++
    terminal().sources() ++
    amm.util().sources() ++
    amm.runtime().sources() ++
    amm.`interp-api`().sources() ++
    amm.`repl-api`().sources() ++
    amm.interp().sources() ++
    amm.repl().sources() ++
    sources() ++
    externalSources()



  def prependShellScriptFor(mainClass: String) = {
    mill.modules.Jvm.launcherUniversalScript(
      mainClass,
      Agg("$0"),
      Agg("%~dpnx0"),
      // G1 Garbage Collector is awesome https://github.com/lihaoyi/Ammonite/issues/216
      Seq("-Xmx500m", "-XX:+UseG1GC")
    )
  }

  def prependShellScript = T{
    prependShellScriptFor(mainClass().get)
  }

  def prependBootstrapShellScript = T{
    prependShellScriptFor("coursier.bootstrap.launcher.ResourcesLauncher")
  }

  def dependencyResourceFileName = "amm-dependencies.txt"

  def bootstrap = T{
    import coursier.bootstrap.{Bootstrap, ClassLoaderContent, ClasspathEntry}

    def pathRefEntry(p: os.Path) =
      if (!p.toIO.exists() || !p.isFile)
        Nil
      else {
        val b = os.read.bytes(p)
        Seq(ClasspathEntry.Resource(p.last, 0L, b))
      }

    val replApiCp = {
      amm.`repl-api`().runClasspath() ++
      amm.`repl-api`().externalSources() ++
      amm.`repl-api`().transitiveJars() ++
      amm.`repl-api`().transitiveSourceJars()
    }

    val ammCp = runClasspath() ++ transitiveJars() ++ transitiveSourceJars()

    val content = Seq(
      ClassLoaderContent(
        replApiCp.distinct.flatMap(r => pathRefEntry(r.path))
      ),
      ClassLoaderContent(
        ammCp.filterNot(replApiCp.toSet).distinct.flatMap(r => pathRefEntry(r.path))
      )
    )

    val dest = T.ctx().dest / "bootstrap-no-preamble.jar"

    val thread = Thread.currentThread()
    val previousCl = thread.getContextClassLoader
    val coursierCl = Bootstrap.getClass.getClassLoader
    try {
      thread.setContextClassLoader(coursierCl)
      Bootstrap.create(
        content,
        mainClass().get,
        dest.toNIO,
        withPreamble = false,
        deterministic = true,
        hybridAssembly = true
      )
    } finally {
      thread.setContextClassLoader(previousCl)
    }

    val finalDest = (T.ctx().dest / "bootstrap.jar")

    os.write(
      finalDest,
      Seq[os.Source](
        prependBootstrapShellScript(),
        os.read.inputStream(dest)
      )
    )

    if (!scala.util.Properties.isWin) {
      import java.nio.file.attribute.PosixFilePermission
      os.perms.set(
        finalDest,
        os.perms(finalDest)
          + PosixFilePermission.GROUP_EXECUTE
          + PosixFilePermission.OWNER_EXECUTE
          + PosixFilePermission.OTHERS_EXECUTE
      )
}

    PathRef(finalDest)
  }

  object test extends CustomRunnerTests{
    def moduleDeps = super.moduleDeps ++ Seq(amm.repl().test)
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.chuusai::shapeless:2.3.3"
    )
    // Need to duplicate this from MainModule due to Mill not properly propagating it through
    def runClasspath =
      super.runClasspath() ++
      ops().sources() ++
      terminal().sources() ++
      amm.util().sources() ++
      amm.runtime().sources() ++
      amm.`interp-api`().sources() ++
      amm.`repl-api`().sources() ++
      amm.interp().sources() ++
      amm.repl().sources() ++
      sources() ++
      externalSources()

  }
}

object shell extends Cross[ShellModule](fullCrossScalaVersions:_*)
class ShellModule(val crossScalaVersion: String) extends AmmModule{
  def moduleDeps = Seq(ops(), amm())
  def crossFullScalaVersion = true
  object test extends CustomRunnerTests{
    def moduleDeps = super.moduleDeps ++ Seq(amm.repl().test)
    def forkEnv = super.forkEnv() ++ Seq(
      "AMMONITE_SHELL" -> shell().jar().path.toString,
      "AMMONITE_ASSEMBLY" -> amm().bootstrap().path.toString
    )
  }
}
object integration extends Cross[IntegrationModule](fullCrossScalaVersions:_*)
class IntegrationModule(val crossScalaVersion: String) extends AmmInternalModule{
  def moduleDeps = Seq(ops(), amm())
  object test extends Tests {
    def forkEnv = super.forkEnv() ++ Seq(
      "AMMONITE_SHELL" -> shell().jar().path.toString,
      "AMMONITE_ASSEMBLY" -> amm().bootstrap().path.toString
    )
  }
}

object sshd extends Cross[SshdModule](fullCrossScalaVersions:_*)
class SshdModule(val crossScalaVersion: String) extends AmmModule{
  def moduleDeps = Seq(ops(), amm())
  def crossFullScalaVersion = true
  def ivyDeps = Agg(
    // sshd-core 1.3.0 requires java8
    ivy"org.apache.sshd:sshd-core:1.2.0",
    ivy"org.bouncycastle:bcprov-jdk15on:1.56",
  )
  object test extends Tests {
    def ivyDeps = super.ivyDeps() ++ Agg(
      // slf4j-nop makes sshd server use logger that writes into the void
      ivy"org.slf4j:slf4j-nop:1.7.12",
      ivy"com.jcraft:jsch:0.1.54",
      ivy"org.scalacheck::scalacheck:1.14.0"
    )
  }
}

def unitTest(scalaVersion: String = sys.env("TRAVIS_SCALA_VERSION")) = T.command{
  ops(scalaVersion).test.test()()
  terminal(scalaVersion).test.test()()
  amm.repl(scalaVersion).test.test()()
  amm(scalaVersion).test.test()()
  shell(scalaVersion).test.test()()
  sshd(scalaVersion).test.test()()
}

def integrationTest(scalaVersion: String = sys.env("TRAVIS_SCALA_VERSION")) = T.command{
  integration(scalaVersion).test.test()()
}

def generateConstantsFile(version: String = buildVersion,
                          unstableVersion: String = "<fill-me-in-in-Constants.scala>",
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

  write(ctx.dest/"Constants.scala", versionTxt)
  ctx.dest/"Constants.scala"
}

def generateDependenciesFile(scalaVersion: String,
                             fileName: String,
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

  mkdir(dir)
  println(s"Writing $dest")
  dir.toIO.mkdirs()
  write(dest, content.getBytes("UTF-8"))

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
        "https://api.github.com/repos/lihaoyi/Ammonite/releases",
        data = ujson.write(
          ujson.Obj(
            "tag_name" -> buildVersion,
            "name" -> buildVersion,
            "body" -> s"http://www.lihaoyi.com/Ammonite/#$buildVersion"
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
        jar.path,
        latestTaggedVersion,
        s"$scalaBinaryVersion-$buildVersion",
        sys.env("AMMONITE_BOT_AUTH_TOKEN")
      )
    }
  }
}

def publishDocs() = {
  // Disable doc auto-publishing for now, as the recent modularization means we
  // need to make significant changes to the readme and that'll time.
  if (!isMasterCommit) T.command{
    println("MISC COMMIT: Building readme for verification")
    %sbt(
      "readme/run",
      AMMONITE_SHELL=shell("2.13.0").jar().path,
      AMMONITE_ASSEMBLY=amm("2.13.0").bootstrap().path,
      CONSTANTS_FILE=generateConstantsFile()
    )
  }else T.command{
    println("MASTER COMMIT: Updating version and publishing to Github Pages")

    val publishDocs = sys.env("DEPLOY_KEY").replace("\\n", "\n")
    write(pwd / 'deploy_key, publishDocs)

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
      s"https://github.com/lihaoyi/Ammonite/releases/download/$stableKey",
      s"https://github.com/lihaoyi/Ammonite/releases/download/$unstableKey",
      for(k <- oldStableKeys)
        yield (k, s"https://github.com/lihaoyi/Ammonite/releases/download/$k"),
      for(k <- oldUnstableKeys)
        yield (k, s"https://github.com/lihaoyi/Ammonite/releases/download/$k")
    )

    %sbt(
      "readme/run",
      AMMONITE_SHELL=shell("2.13.0").jar().path,
      AMMONITE_ASSEMBLY=amm("2.13.0").bootstrap().path,
      CONSTANTS_FILE=constantsFile
    )
    %("ci/deploy_master_docs.sh")
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
  if (!isMasterCommit) T.command{()}
  else T.command{

    write(pwd/"gpg_key", sys.env("SONATYPE_PGP_KEY_CONTENTS").replace("\\n", "\n"))
    %("gpg", "--import", "gpg_key")
    rm(pwd/"gpg_key")

    val x: Seq[(Seq[(Path, String)], Artifact)] = {
      mill.define.Task.sequence(partition(publishArtifacts, shard, divisionCount))().map{
        case PublishModule.PublishData(a, s) => (s.map{case (p, f) => (p.path, f)}, a)
      }
    }

    new SonatypePublisher(
      "https://oss.sonatype.org/service/local",
      "https://oss.sonatype.org/content/repositories/snapshots",
      sys.env("SONATYPE_DEPLOY_USER") + ":" + sys.env("SONATYPE_DEPLOY_PASSWORD"),
      Option(sys.env("SONATYPE_PGP_PASSWORD")),
      None,
      true,
      60000,
      60000,
      T.ctx().log
    ).publishAll(
      true,
      x:_*
    )
  }

