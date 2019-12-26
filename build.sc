import mill._, scalalib._, publish._
import ammonite.ops._, ImplicitWd._
import $file.ci.upload

val isMasterCommit =
  sys.env.get("TRAVIS_PULL_REQUEST") == Some("false") &&
  (sys.env.get("TRAVIS_BRANCH") == Some("master") || sys.env("TRAVIS_TAG") != "")

val latestTaggedVersion = os.proc('git, 'describe, "--abbrev=0", "--tags").call().out.trim

val commitsSinceTaggedVersion = {
  os.proc('git, "rev-list", 'master, "--not", latestTaggedVersion, "--count")
    .call()
    .out
    .trim
    .toInt
}


val binCrossScalaVersions = Seq("2.12.10", "2.13.1")
val fullCrossScalaVersions = Seq(
  "2.12.1", "2.12.2", "2.12.3", "2.12.4", "2.12.6", "2.12.7", "2.12.8", "2.12.9", "2.12.10",
  "2.13.0", "2.13.1"
)

val latestAssemblies = binCrossScalaVersions.map(amm(_).assembly)

val (buildVersion, unstable) = sys.env.get("TRAVIS_TAG") match{
  case Some(v) if v != "" => (v, false)
  case _ =>
    val gitHash = os.proc("git", "rev-parse", "--short", "HEAD").call().out.trim
    (s"$latestTaggedVersion-$commitsSinceTaggedVersion-${gitHash}", true)
}

trait AmmInternalModule extends mill.scalalib.CrossSbtModule{
  def artifactName = "ammonite-" + millOuterCtx.segments.parts.mkString("-").stripPrefix("amm-")
  def testFramework = "utest.runner.Framework"
  def scalacOptions = Seq("-P:acyclic:force")
  def compileIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.2.0")
  def scalacPluginIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.2.0")
  trait Tests extends super.Tests{
    def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.1")
    def testFrameworks = Seq("utest.runner.Framework")
    def forkArgs = Seq("-Xmx2g", "-Dfile.encoding=UTF8")
  }
  def allIvyDeps = T{transitiveIvyDeps() ++ scalaLibraryIvyDeps()}
  def sources = T.sources{
    val sv = scalaVersion()
    val extraDir =
      if (sv.startsWith("2.12.")) {
        val patch = sv.stripPrefix("2.12.").takeWhile(_.isDigit).toInt
        val dirName =
          if (patch <= 8)
            "scala-2.12.0_8"
          else
            "scala-2.12.9+"
        Seq(PathRef(millSourcePath / "src" / "main" / dirName))
      } else
        Nil

    val extraDir2 = PathRef(
      if (sv == "2.12.10" || sv == "2.13.1") millSourcePath / "src" / "main" / "scala-2.12.10-2.13.1+"
      else millSourcePath / "src" / "main" / "scala-not-2.12.10-2.13.1+"
    )
    val extraDir3 =
      if (sv == "2.13.1") Nil
      else Seq(PathRef(millSourcePath / "src" / "main" / "scala-not-2.13.1"))

    super.sources() ++ extraDir ++ Seq(extraDir2) ++ extraDir3
  }
  def externalSources = T{
    resolveDeps(allIvyDeps, sources = true)()
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
  def dependencyFileResources = T{
    val deps0 = T.task{compileIvyDeps() ++ transitiveIvyDeps()}()
    val (_, res) = mill.modules.Jvm.resolveDependenciesMetadata(
      repositories,
      deps0.map(resolveCoursierDependency().apply(_)),
      deps0.filter(_.force).map(resolveCoursierDependency().apply(_)),
      mapDependencies = Some(mapDependencies())
    )


    Seq(PathRef(generateDependenciesFile(
      crossScalaVersion,
      dependencyResourceFileName,
      res.minDependencies.toSeq
    )))
  }

  def resources = T.sources {
    super.resources() ++ dependencyFileResources()
  }
}

object ops extends Cross[OpsModule](binCrossScalaVersions:_*)
class OpsModule(val crossScalaVersion: String) extends AmmModule{
  def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.6.2",
    ivy"org.scala-lang.modules::scala-collection-compat:2.1.2"
  )
  def scalacOptions = super.scalacOptions().filter(!_.contains("acyclic"))
  object test extends Tests
}

object terminal extends Cross[TerminalModule](binCrossScalaVersions:_*)
class TerminalModule(val crossScalaVersion: String) extends AmmModule{
  def ivyDeps = Agg(
    ivy"com.lihaoyi::sourcecode:0.1.8",
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
      ivy"com.lihaoyi::pprint:0.5.7",
      ivy"com.lihaoyi::fansi:0.2.7",
    )
    def compileIvyDeps = Agg(
      ivy"org.scala-lang:scala-reflect:$crossScalaVersion"
    )
  }

  object runtime extends Cross[RuntimeModule](fullCrossScalaVersions:_*)
  class RuntimeModule(val crossScalaVersion: String) extends AmmModule{
    def moduleDeps = Seq(ops(), amm.util(), interp.api(), amm.repl.api())
    def crossFullScalaVersion = true
    def ivyDeps = Agg(
      ivy"com.lihaoyi::upickle:0.9.5",
      ivy"com.lihaoyi::requests:0.4.6"
    )
  }

  object interp extends Cross[InterpModule](fullCrossScalaVersions:_*){
    object api extends Cross[InterpApiModule](fullCrossScalaVersions:_*)
    class InterpApiModule(val crossScalaVersion: String) extends AmmModule{
      def moduleDeps = Seq(ops(), amm.util())
      def crossFullScalaVersion = true
      def ivyDeps = Agg(
        ivy"org.scala-lang:scala-compiler:$crossScalaVersion",
        ivy"org.scala-lang:scala-reflect:$crossScalaVersion",
        ivy"io.get-coursier:interface:0.0.8"
      )
    }
  }
  class InterpModule(val crossScalaVersion: String) extends AmmModule{
    def moduleDeps = Seq(ops(), amm.util(), amm.runtime())
    def crossFullScalaVersion = true
    def ivyDeps = Agg(
      ivy"org.scala-lang:scala-compiler:$crossScalaVersion",
      ivy"org.scala-lang:scala-reflect:$crossScalaVersion",
      ivy"com.lihaoyi::scalaparse:2.2.2",
      ivy"org.javassist:javassist:3.21.0-GA",
      ivy"org.scala-lang.modules::scala-xml:1.2.0"
    )
  }

  object `test-runner` extends mill.scalalib.SbtModule {
    def scalaVersion = "2.12.8"
    def ivyDeps = Agg(
      ivy"com.lihaoyi::mill-scalalib:${sys.props("MILL_VERSION")}"
    )
  }

  object repl extends Cross[ReplModule](fullCrossScalaVersions:_*){

    object api extends Cross[ReplApiModule](fullCrossScalaVersions:_*)
    class ReplApiModule(val crossScalaVersion: String) extends AmmModule with AmmDependenciesResourceFileModule{
      def crossFullScalaVersion = true
      def dependencyResourceFileName = "amm-dependencies.txt"
      def moduleDeps = Seq(
        ops(), amm.util(),
        interp.api()
      )
      def ivyDeps = Agg(
        ivy"com.github.scopt::scopt:3.7.1"
      )

      def generatedSources = T{
        Seq(PathRef(generateConstantsFile(buildVersion)))
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

    object test extends Tests with AmmDependenciesResourceFileModule {
      def crossScalaVersion = ReplModule.this.crossScalaVersion
      def scalaVersion = ReplModule.this.crossScalaVersion
      def dependencyResourceFileName = "amm-test-dependencies.txt"

      def thinWhitelist = T{
        generateApiWhitelist(
          amm.repl.api().exposedClassPath() ++
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
      def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"org.scalaz::scalaz-core:7.2.27"
      )
    }
  }
}

class MainModule(val crossScalaVersion: String)
  extends AmmModule {

  def artifactName = "ammonite"

  def crossFullScalaVersion = true

  def mainClass = Some("ammonite.Main")

  def moduleDeps = Seq(
    terminal(), ops(),
    amm.util(), amm.runtime(),
    amm.interp.api(),
    amm.repl.api(),
    amm.interp(), amm.repl()
  )

  def runClasspath =
    super.runClasspath() ++
    ops().sources() ++
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
      amm.repl.api().exposedClassPath()
    )
  }
  def localClasspath = T{
    super.localClasspath() ++ Agg(thinWhitelist())
  }

  object test extends Tests{
    def moduleDeps = super.moduleDeps ++ Seq(amm.repl().test)
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.chuusai::shapeless:2.3.3"
    )


    def thinWhitelist = T{
      generateApiWhitelist(
        amm.repl.api().exposedClassPath() ++
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
      ops().sources() ++
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

object shell extends Cross[ShellModule](fullCrossScalaVersions:_*)
class ShellModule(val crossScalaVersion: String) extends AmmModule{
  def moduleDeps = Seq(ops(), amm())
  def crossFullScalaVersion = true
  object test extends Tests{
    def moduleDeps = super.moduleDeps ++ Seq(amm.repl().test)
    def thinWhitelist = T{
      generateApiWhitelist(
        amm.repl.api().exposedClassPath() ++
        Seq(amm.repl().test.compile().classes, compile().classes) ++
        resolveDeps(T.task{compileIvyDeps() ++ transitiveIvyDeps()})()
      )
    }

    def localClasspath = T{
      super.localClasspath() ++ Agg(thinWhitelist())
    }
    def forkEnv = super.forkEnv() ++ Seq(
      "AMMONITE_SHELL" -> shell().jar().path.toString,
      "AMMONITE_ASSEMBLY" -> amm().assembly().path.toString
    )
  }
}
object integration extends Cross[IntegrationModule](fullCrossScalaVersions:_*)
class IntegrationModule(val crossScalaVersion: String) extends AmmInternalModule{
  def moduleDeps = Seq(ops(), amm())
  object test extends Tests {
    def forkEnv = super.forkEnv() ++ Seq(
      "AMMONITE_SHELL" -> shell().jar().path.toString,
      "AMMONITE_ASSEMBLY" -> amm().launcher().path.toString
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
      upload(
        os.temp(
          os.read(os.pwd / "amm-template.sh")
            .replace("DEFAULT_AMM_VERSION=", s"DEFAULT_AMM_VERSION=$latestTaggedVersion")
        ),
        latestTaggedVersion,
        s"$scalaBinaryVersion-$buildVersion-bootstrap",
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
      AMMONITE_SHELL=shell("2.13.1").jar().path,
      AMMONITE_ASSEMBLY=amm("2.13.1").assembly().path,
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
      AMMONITE_SHELL=shell("2.13.1").jar().path,
      AMMONITE_ASSEMBLY=amm("2.13.1").assembly().path,
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
      120000,
      120000,
      T.ctx().log,
      120000
    ).publishAll(
      true,
      x:_*
    )
  }

