package ammonite.repl

import org.apache.ivy.Ivy
import org.apache.ivy.core.module.descriptor.{DefaultDependencyDescriptor, DefaultModuleDescriptor}
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.report.DownloadStatus
import org.apache.ivy.core.resolve.{IvyNode, ResolveOptions}
import org.apache.ivy.core.settings.IvySettings
import org.apache.ivy.plugins.repository.file.FileRepository
import org.apache.ivy.util._

import org.apache.ivy.plugins.resolver.{ChainResolver, FileSystemResolver, IBiblioResolver}
import acyclic.file
object IvyConstructor extends IvyConstructor
trait IvyConstructor{
  implicit class GroupIdExt(groupId: String){
    def %(artifactId: String) = (groupId, artifactId)
    def %%(artifactId: String) = (groupId, artifactId + "_" + IvyThing.scalaBinaryVersion)
  }
  implicit class ArtifactIdExt(t: (String, String)){
    def %(version: String) = (t._1, t._2, version)
  }
}

/**
 * Resolve artifacts from Ivy. Originally taken from
 *
 * http://makandracards.com/evgeny-goldin/5817-calling-ivy-from-groovy-or-java
 *
 * And transliterated into Scala. I have no idea how or why it works.
 */
object IvyThing {
  case class IvyResolutionException(failed: Seq[String]) extends Exception(
    "failed to resolve ivy dependencies " + failed.mkString(", ")
  )
  val scalaBinaryVersion =
    scala.util.Properties
              .versionString
              .stripPrefix("version ")
              .split('.')
              .take(2)
              .mkString(".")

  var maxLevel = 2
  Message.setDefaultLogger(new AbstractMessageLogger {
    def doEndProgress(msg: String) = Console.err.println("Done")
    def doProgress() = Console.err.print(".")
    def log(msg: String, level: Int) =  if (level <= maxLevel) Console.err.println(msg)
    def rawlog(msg: String, level: Int) = log(msg, level)
  })

  def resolveArtifact(groupId: String,
                      artifactId: String,
                      version: String,
                      verbosity: Int = 2) = synchronized {
    maxLevel = verbosity
    val ivy = Ivy.newInstance {

      def resolver(name: String) = {
        val res = new IBiblioResolver()
        res.setUsepoms(true)
        res.setM2compatible(true)
        res.setName(name)
        res.setRoot("http://repo1.maven.org/maven2/")
        res
      }
      def fileResolver(name: String, root: String, pattern: String, m2: Boolean = false) = {
        val testRepoDir = sys.props("user.home") + root
        val repo = new FileRepository(new java.io.File(testRepoDir))

        val res = new FileSystemResolver()
        res.addIvyPattern(testRepoDir + pattern)
        res.addArtifactPattern(testRepoDir + pattern)
        res.setRepository(repo)
        res.setM2compatible(m2)
        res.setName(name)

        res
      }

      //add duplicate resolvers with different name to make Ivy shut up
      //and stop giving `unknown resolver null` or `unknown resolver sbt-chain`
      //errors


      val resolvers = Seq(
        fileResolver(
          "cache",
          "/.ivy2/cache",
          "/[organisation]/[module]/jars/[artifact]-[revision].[ext]"
        ),
        fileResolver(
          "local",
          "/.ivy2/local",
          "/[organisation]/[module]/[revision]/jars/[artifact].[ext]"
        ),
        fileResolver(
          "m2",
          "/.m2/repository",
          "/[organisation]/[module]/[revision]/[artifact]-[revision].[ext]",
          m2 = true
        ),
        resolver("central")
      )

      //creates clear ivy settings
      val ivySettings = new IvySettings()
      //adding maven repo resolver
      val chainResolver = new ChainResolver
      chainResolver.setReturnFirst(true)
      resolvers.foreach(chainResolver.add)
      ivySettings.addResolver(chainResolver)

      //set to the default resolver
      ivySettings.setDefaultResolver(chainResolver.getName)
      //creates an Ivy instance with settings
      ivySettings
    }

    val md = DefaultModuleDescriptor.newDefaultInstance(
      ModuleRevisionId.newInstance(
        groupId,
        artifactId + "-caller",
        "working"
      )
    )

    md.addDependency {
      val desc = new DefaultDependencyDescriptor(
        md,
        ModuleRevisionId.newInstance(groupId, artifactId, version),
        false,
        false,
        true
      )
      // No idea wtf this is
      desc.addDependencyConfiguration("*", "*")
      desc
    }

    val options = new ResolveOptions()
      .setConfs(Array("default"))
      .setRefresh(true)
      .setOutputReport(false)

    //init resolve report
    val report = ivy.resolve(md, options)
    val unresolved = report.getAllProblemMessages

    import collection.JavaConversions._

    if (unresolved.size == 0) report.getAllArtifactsReports.map(_.getLocalFile)
    else throw IvyResolutionException(unresolved.toSeq.map(_.toString))
  }
}
