package ammonite.repl

import java.io.{IOException, File}
import java.net.{URISyntaxException, URL}
import java.util.Collections

import org.apache.ivy.Ivy
import org.apache.ivy.core.module.descriptor.{Artifact, DependencyDescriptor, DefaultDependencyDescriptor, DefaultModuleDescriptor}
import org.apache.ivy.core.module.id.ModuleRevisionId
import org.apache.ivy.core.resolve.{ResolveData, ResolveOptions}
import org.apache.ivy.core.settings.IvySettings
import org.apache.ivy.plugins.repository.{TransferEvent, RepositoryCopyProgressListener}
import org.apache.ivy.plugins.repository.file.{FileRepository, FileResource}
import org.apache.ivy.plugins.repository.url.URLRepository
import org.apache.ivy.plugins.resolver._
import org.apache.ivy.util._

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
  val scalaBinaryVersion = scala.util.Properties.versionString
    .stripPrefix("version ").split('.').take(2).mkString(".")

  def resolveArtifact(groupId: String, artifactId: String, version: String) = {

    val ivy = Ivy.newInstance{
      val resolver = {
        val res = new IBiblioResolver()
        res.setUsepoms(true)
        //      res.setUseMavenMetadata(true)
        res.setM2compatible(true)
        res.setName("central")
        res.setRoot("http://repo1.maven.org/maven2/")
        res
      }

      //creates clear ivy settings
      val ivySettings = new IvySettings()
      //adding maven repo resolver
      ivySettings.addResolver(resolver)

      println("ivySettings.getResolverNames " + ivySettings.getResolverNames)
      //set to the default resolver
      ivySettings.setDefaultResolver(resolver.getName)
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

    md.addDependency{
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
    //so you can get the jar libraries
    report.getAllArtifactsReports.map(_.getLocalFile)
  }
}


/*
 * ResolverHelpers and Resolver content is some sbt code, a tiny bit refactored
 */

object ResolverHelpers {

  object ChecksumFriendlyURLResolver {
    import java.lang.reflect.AccessibleObject
    def reflectiveLookup[A <: AccessibleObject](f: Class[_] => A): Option[A] =
      try {
        val cls = classOf[RepositoryResolver]
        val thing = f(cls)
        import scala.language.reflectiveCalls
        thing.setAccessible(true)
        Some(thing)
      } catch {
        case (_: java.lang.NoSuchFieldException) |
             (_: java.lang.SecurityException) |
             (_: java.lang.NoSuchMethodException) => None
      }
    val signerNameField: Option[java.lang.reflect.Field] =
      reflectiveLookup(_.getDeclaredField("signerName"))
    val putChecksumMethod: Option[java.lang.reflect.Method] =
      reflectiveLookup(_.getDeclaredMethod("putChecksum",
        classOf[Artifact], classOf[File], classOf[String],
        classOf[Boolean], classOf[String]))
    val putSignatureMethod: Option[java.lang.reflect.Method] =
      reflectiveLookup(_.getDeclaredMethod("putSignature",
        classOf[Artifact], classOf[File], classOf[String],
        classOf[Boolean]))
  }

  trait ChecksumFriendlyURLResolver extends RepositoryResolver {
    import ChecksumFriendlyURLResolver._
    def signerName: String = signerNameField match {
      case Some(field) => field.get(this).asInstanceOf[String]
      case None        => null
    }
    override protected def put(artifact: Artifact, src: File, dest: String, overwrite: Boolean) = {
      // verify the checksum algorithms before uploading artifacts!
      val checksums = getChecksumAlgorithms
      val repository = getRepository
      for {
        checksum <- checksums
        if !ChecksumHelper.isKnownAlgorithm(checksum)
      } throw new IllegalArgumentException("Unknown checksum algorithm: " + checksum)
      repository.put(artifact, src, dest, overwrite)
      // Fix for sbt#1156 - Artifactory will auto-generate MD5/sha1 files, so
      // we need to overwrite what it has.
      for (checksum <- checksums) {
        putChecksumMethod match {
          case Some(method) => method.invoke(this, artifact, src, dest, true: java.lang.Boolean, checksum)
          case None         => // TODO - issue warning?
        }
      }
      if (signerName != null) {
        putSignatureMethod match {
          case None         => ()
          case Some(method) => method.invoke(artifact, src, dest, true: java.lang.Boolean)
        }
      }
    }
  }

  trait DescriptorRequired extends BasicResolver {
    override def getDependency(dd: DependencyDescriptor, data: ResolveData) = {
      val prev = descriptorString(isAllownomd)
      setDescriptor(descriptorString(hasExplicitURL(dd)))
      try super.getDependency(dd, data) finally setDescriptor(prev)
    }
    def descriptorString(optional: Boolean) =
      if (optional) BasicResolver.DESCRIPTOR_OPTIONAL else BasicResolver.DESCRIPTOR_REQUIRED
    def hasExplicitURL(dd: DependencyDescriptor) =
      dd.getAllDependencyArtifacts.exists(_.getUrl != null)
  }

  final class WarnOnOverwriteFileRepo extends FileRepository() {
    override def put(source: java.io.File, destination: String, overwrite: Boolean) = {
      try super.put(source, destination, overwrite)
      catch {
        case e: IOException if e.getMessage.contains("destination already exists") =>
          Message.warn(s"Attempting to overwrite $destination\n\tThis usage is deprecated and will be removed in sbt 1.0.")
          super.put(source, destination, true)
      }
    }
  }

  final class LocalIfFileRepo extends URLRepository {
    val repo = new WarnOnOverwriteFileRepo()
    val progress = new RepositoryCopyProgressListener(this)
    override def getResource(source: String) = {
      val url = new URL(source)
      if (url.getProtocol == "file")
        new FileResource(repo, try { new File(url.toURI) } catch { case _: URISyntaxException => new File(url.getPath) })
      else
        super.getResource(source)
    }

    override def put(source: File, destination: String, overwrite: Boolean) = {
      val url = new URL(destination)
      if (url.getProtocol != "file") super.put(source, destination, overwrite)
      else {
        // Here we duplicate the put method for files so we don't just bail on trying ot use Http handler
        val resource = getResource(destination)
        if (!overwrite && resource.exists()) {
          throw new IOException("destination file exists and overwrite == false")
        }
        fireTransferInitiated(resource, TransferEvent.REQUEST_PUT)
        try {
          val totalLength = source.length
          if (totalLength > 0) progress.setTotalLength(totalLength)
          FileUtil.copy(source, new File(url.toURI), progress)
        } catch {
          case ex: IOException =>
            fireTransferError(ex)
            throw ex
          case ex: RuntimeException =>
            fireTransferError(ex)
            throw ex
        } finally {
          progress.setTotalLength(null)
        }
      }
    }
  }

  def resolvePattern(base: String, pattern: String): String = {
    val normBase = base.replace('\\', '/')
    if (normBase.endsWith("/") || pattern.startsWith("/")) normBase + pattern else normBase + "/" + pattern
  }

  val mavenStyleBasePattern = "[organisation]/[module](_[scalaVersion])(_[sbtVersion])/[revision]/[artifact]-[revision](-[classifier]).[ext]"

  def initializeMavenStyle(resolver: IBiblioResolver, name: String, root: String) {
    resolver.setName(name)
    resolver.setM2compatible(true)
    resolver.setRoot(root)
  }

  case class Patterns(ivyPatterns: Seq[String], artifactPatterns: Seq[String], isMavenCompatible: Boolean, descriptorOptional: Boolean = false, skipConsistencyCheck: Boolean = false)

  def initializePatterns(resolver: AbstractPatternsBasedResolver, patterns: Patterns, settings: IvySettings = new IvySettings()) {
    resolver.setM2compatible(patterns.isMavenCompatible)
    resolver.setDescriptor(if (patterns.descriptorOptional) BasicResolver.DESCRIPTOR_OPTIONAL else BasicResolver.DESCRIPTOR_REQUIRED)
    resolver.setCheckconsistency(!patterns.skipConsistencyCheck)
    patterns.ivyPatterns.foreach(p => resolver.addIvyPattern(settings substitute p))
    patterns.artifactPatterns.foreach(p => resolver.addArtifactPattern(settings substitute p))
  }

  val userHome = new File(System.getProperty("user.home"))

  def mavenLocalDir: File = {
    def loadHomeFromSettings(f: () => File): Option[File] =
      try {
        val file = f()
        if (!file.exists) None
        else (scala.xml.XML.loadFile(file) \ "localRepository").text match {
          case ""    => None
          case e @ _ => Some(new File(e))
        }
      } catch {
        // Occurs inside File constructor when property or environment variable does not exist
        case _: NullPointerException => None
        // Occurs when File does not exist
        case _: IOException          => None
        case e: org.xml.sax.SAXParseException    => System.err.println(s"WARNING: Problem parsing ${f().getAbsolutePath}, ${e.getMessage}"); None
      }
    loadHomeFromSettings(() => new File(userHome, ".m2/settings.xml")) orElse
      loadHomeFromSettings(() => new File(new File(System.getenv("M2_HOME")), "conf/settings.xml")) getOrElse
      new File(userHome, ".m2/repository")
  }

  val SonatypeRepositoryRoot = "https://oss.sonatype.org/content/repositories"

  def centralRepositoryRoot(secure: Boolean) = (if (secure) "https" else "http") + "://repo1.maven.org/maven2/"

  def fileRepository(name: String, patterns: Patterns, isLocal: Boolean, isTransactional: Option[Boolean], settings: IvySettings = new IvySettings()): DependencyResolver = {
    val resolver = new FileSystemResolver with DescriptorRequired {
      // Workaround for #1156
      // Temporarily in sbt 0.13.x we deprecate overwriting
      // in local files for non-changing revisions.
      // This will be fully enforced in sbt 1.0.
      setRepository(new WarnOnOverwriteFileRepo())
    }
    resolver.setName(name)
    initializePatterns(resolver, patterns, settings)
    resolver.setLocal(isLocal)
    isTransactional.foreach(value => resolver.setTransactional(value.toString))
    resolver
  }

  val PluginPattern = "(scala_[scalaVersion]/)(sbt_[sbtVersion]/)"
  val localBasePattern = "[organisation]/[module]/" + PluginPattern + "[revision]/[type]s/[artifact](-[classifier]).[ext]"

  val ivyHome = new File(userHome, ".ivy2")
}

object Resolver {
  import ResolverHelpers._

  def mavenResolver(name: String, root: String): DependencyResolver = {
    val pattern = Collections.singletonList(resolvePattern(root, mavenStyleBasePattern))
    final class PluginCapableResolver extends IBiblioResolver with ChecksumFriendlyURLResolver with DescriptorRequired {
      def setPatterns() {
        // done this way for access to protected methods.
        setArtifactPatterns(pattern)
        setIvyPatterns(pattern)
      }
    }
    val resolver = new PluginCapableResolver
    resolver.setRepository(new LocalIfFileRepo)
    initializeMavenStyle(resolver, name, root)
    resolver.setPatterns() // has to be done after initializeMavenStyle, which calls methods that overwrite the patterns
    resolver
  }

  val mavenLocal = mavenResolver("Maven2 Local", mavenLocalDir.toURI.toURL.toString)

  val defaultMaven = mavenResolver("public", centralRepositoryRoot(secure = true))

  def sonatypeRepo(status: String) = mavenResolver("sonatype-" + status, SonatypeRepositoryRoot + "/" + status)

  lazy val localRepo = {
    val id = "local"
    val pList = List(s"${ivyHome.getAbsolutePath}/$id/$localBasePattern")
    fileRepository(id, Patterns(pList, pList, isMavenCompatible = false), isLocal = true, isTransactional = None)
  }
}
