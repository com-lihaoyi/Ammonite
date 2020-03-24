package ammonite.runtime

import java.nio.file.FileAlreadyExistsException
import java.nio.file.{Files, Paths}

import ammonite.repl.api.History
import ammonite.util._
import ammonite.util.Util._
import coursierapi.{Dependency, Module}

import scala.util.Try
import scala.collection.mutable
import scala.reflect.NameTransformer.encode


/**
 * Trait for the interface of common persistent storage. This handles history
 * and persistent caches. Right now it is not threadsafe nor does it handle
 * the mutual exclusion of files between processes. Mutexes should be added
 * to be able to run multiple Ammonite processes on the same system.
 */
trait Storage{
  def loadPredef: Option[(String, os.Path)]
  val fullHistory: StableRef[History]
  val ivyCache: StableRef[Storage.IvyMap]

  def compileCacheSave(path: String, tag: Tag, data: Storage.CompileCache): Unit
  def compileCacheLoad(path: String, tag: Tag): Option[Storage.CompileCache]
  def classFilesListSave(filePathPrefix: os.SubPath,
                         perBlockMetadata: Seq[ScriptOutput.BlockMetadata],
                         tag: Tag): Unit
  def classFilesListLoad(filePathPrefix: os.SubPath, tag: Tag): Option[ScriptOutput]
  def getSessionId: Long
}

object Storage{
  case class CompileCache(classFiles: Vector[(String, Array[Byte])], imports: Imports)
  type IvyMap = Map[(String, Seq[Dependency]), Seq[String]]

  private final case class DependencyLike(
    module: DependencyLike.ModuleLike,
    version: String,
    exclusions: Set[(String, String)],
    configuration: String,
    `type`: String,
    classifier: String,
    transitive: Boolean
  ) {
    def dependency: Dependency = {
      val dep = Dependency.of(module.module, version)
        .withConfiguration(configuration)
        .withType(`type`)
        .withClassifier(classifier)
        .withTransitive(transitive)
      for ((o, n) <- exclusions)
        dep.addExclusion(o, n)
      dep
    }
  }
  private object DependencyLike {
    import scala.collection.JavaConverters._
    final case class ModuleLike(org: String, name: String, attributes: Map[String, String]) {
      def module: Module =
        Module.of(org, name, attributes.asJava)
    }
    def apply(dependency: Dependency): DependencyLike =
      DependencyLike(
        ModuleLike(
          dependency.getModule.getOrganization,
          dependency.getModule.getName,
          dependency.getModule.getAttributes.asScala.toMap
        ),
        dependency.getVersion,
        dependency.getExclusions.asScala.map(e => (e.getKey, e.getValue)).toSet,
        dependency.getConfiguration,
        dependency.getType,
        dependency.getClassifier,
        dependency.isTransitive
      )
  }

  implicit def depRW: upickle.default.ReadWriter[Dependency] = {
    implicit def moduleLike: upickle.default.ReadWriter[DependencyLike.ModuleLike] =
      upickle.default.macroRW
    def helper: upickle.default.ReadWriter[DependencyLike] = upickle.default.macroRW
    helper.bimap(DependencyLike.apply(_), _.dependency)
  }
  implicit def tagRW: upickle.default.ReadWriter[Tag] = upickle.default.macroRW
  /**
    * Read/write [[Name]]s as unboxed strings, in order to save verbosity
    * in the JSON cache files as well as improving performance of
    * reading/writing since we read/write [[Name]]s a *lot*.
    */
  implicit val nameRW: upickle.default.ReadWriter[Name] =
    upickle.default.readwriter[String].bimap[Name](
      name => name.raw,
      raw => Name(raw)
  )
  implicit def importTreeRW: upickle.default.ReadWriter[ImportTree] = upickle.default.macroRW
  implicit def versionedWrapperIdRW: upickle.default.ReadWriter[VersionedWrapperId] =
    upickle.default.macroRW
  implicit def blockMetadataRW: upickle.default.ReadWriter[ScriptOutput.BlockMetadata] =
    upickle.default.macroRW
  implicit def metadataRW: upickle.default.ReadWriter[ScriptOutput.Metadata] =
    upickle.default.macroRW
  implicit def importHookInfoRW: upickle.default.ReadWriter[ImportHookInfo] =
    upickle.default.macroRW
  implicit val importDataRW: upickle.default.ReadWriter[ImportData] = upickle.default.macroRW
  implicit val importTypeRW: upickle.default.ReadWriter[ImportData.ImportType] =
    upickle.default.macroRW
  implicit val importsRW: upickle.default.ReadWriter[Imports] =
    upickle.default.readwriter[Seq[ImportData]].bimap[Imports](
      imports => imports.value,
      data => Imports(data)
  )

  private def loadIfTagMatches(loadedTag: Tag,
                               cacheTag: Tag,
                               classFilesList: Seq[ScriptOutput.BlockMetadata],
                               compileCacheLoad: (String, Tag) => Option[CompileCache]) = {
    if (loadedTag != cacheTag) None
    else{
      val res =
        for(blockMeta <- classFilesList)
        yield compileCacheLoad(blockMeta.id.wrapperPath, blockMeta.id.tag)

      if (res.exists(_.isEmpty)) None
      else Some(ScriptOutput(ScriptOutput.Metadata(classFilesList), res.flatten.map(_.classFiles)))
    }

  }
  case class InMemory() extends Storage{
    var predef = ""
    var sharedPredef = ""
    def loadPredef = None
    def getSessionId = 0L
    var _history = new History(Vector())
    val fullHistory = new StableRef[History]{
      def apply() = _history
      def update(h: History): Unit = _history = h
    }

    var _ivyCache: IvyMap = Map.empty
    val ivyCache = new StableRef[IvyMap]{
      def apply() = _ivyCache
      def update(value: IvyMap): Unit = _ivyCache = value
    }

    var compileCache: mutable.Map[String, (Tag, CompileCache)] = mutable.Map.empty
    val classFilesListcache = {
      mutable.Map.empty[String, (Tag, Seq[ScriptOutput.BlockMetadata])]
    }
    def compileCacheSave(path: String, tag: Tag, data: CompileCache): Unit = {
      compileCache(path) = (tag, data)
    }
    def compileCacheLoad(path: String, tag: Tag) = {
      for {
        (loadedTag, data) <- compileCache.get(path)
        if loadedTag == tag
      } yield data
    }

    def classFilesListSave(filePathPrefix: os.SubPath,
                           perBlockMetadata: Seq[ScriptOutput.BlockMetadata],
                           tag: Tag): Unit = {

      classFilesListcache(filePathPrefix.toString) = (tag, perBlockMetadata)
    }

    def classFilesListLoad(filePathPrefix: os.SubPath,
                           cacheTag: Tag): Option[ScriptOutput] = {

      classFilesListcache.get(filePathPrefix.toString) match{
        case None => None
        case Some((loadedTag, classFilesList)) =>
          loadIfTagMatches(loadedTag, cacheTag, classFilesList, compileCacheLoad)
      }
    }
  }



  class Folder(val dir: os.Path, isRepl: Boolean = true) extends Storage{
    def predef = if (isRepl) dir/"predef.sc" else dir/"predefScript.sc"
    // Each version puts its cache in a separate folder, to bust caches
    // on every version bump; otherwise binary-incompatible changes to
    // ReplAPI/Preprocessor/ammonite-ops will cause scripts to fail after
    // someone upgrades Ammonite.
    val cacheDir = dir/'cache/ammonite.Constants.version
    val compileCacheDir = cacheDir/'compile
    val classFilesOrder = "classFilesOrder.json"
    val ivyCacheFile = cacheDir/"ivycache.json"
    val coursierFetchCacheDir = cacheDir/"coursier-fetch-cache"
    val metadataFile = "metadata.json"
    val sessionFile  = dir/"session"

    def getSessionId() = {
      try os.read(sessionFile).toLong
      catch{case e: Throwable =>
        val randomId = math.abs(util.Random.nextLong)
        os.write.over(sessionFile, randomId.toString, createFolders = true)
        randomId
      }
    }
    val historyFile = dir/'history
    val fullHistory = new StableRef[History]{
      def apply(): History = {
        try{
          new History(upickle.default.read[Vector[String]](os.read(historyFile)))
        }catch{case e: Exception =>
          new History(Vector())
        }
      }

      def update(t: History): Unit = {
        os.write.over(historyFile, upickle.default.stream(t.toVector, indent = 4))
      }
    }


    def classFilesListSave(filePathPrefix: os.SubPath,
                           perBlockMetadata: Seq[ScriptOutput.BlockMetadata],
                           tag: Tag): Unit = {

      val codeCacheDir =
        cacheDir/'scriptCaches/filePathPrefix/tag.code/tag.env/tag.classPathWhitelistHash

      os.makeDir.all(codeCacheDir)
      try {
        os.write.over(
          codeCacheDir/classFilesOrder,
          upickle.default.stream((tag, perBlockMetadata), indent = 4)
        )
      } catch {
        case _: FileAlreadyExistsException => // ignore
        case t: Throwable => throw t
      }
    }

    def readJson[T: upickle.default.Reader](path: os.Path): Option[T] = {
      try {
        val fileData = os.read(path)
        val parsed = upickle.default.read[T](fileData)
        Some(parsed)
      }
      catch{ case e: Throwable => None }
    }

    def classFilesListLoad(filePathPrefix: os.SubPath,
                           tag: Tag): Option[ScriptOutput] = {

      val codeCacheDir =
        cacheDir/'scriptCaches/filePathPrefix/tag.code/tag.env/tag.classPathWhitelistHash

      if(!os.exists(codeCacheDir)) None
      else {
        val metadataJson = readJson[(Tag, Seq[ScriptOutput.BlockMetadata])](
          codeCacheDir/classFilesOrder
        )

        metadataJson match{
          case Some(metadata) =>
            val (loadedTag, classFilesList) = metadata
            loadIfTagMatches(loadedTag, tag, classFilesList, compileCacheLoad)

          case _ => None
        }
      }


    }

    def compileCacheSave(path: String, tag: Tag, data: CompileCache): Unit = {
      val tagCacheDir = {
        compileCacheDir/path.split('.').map(encode)/tag.code/tag.env/tag.classPathWhitelistHash
      }

      os.makeDir.all(tagCacheDir)
      os.write.over(
        tagCacheDir/metadataFile,
        upickle.default.stream((tag, data.imports), indent = 4)
      )
      data.classFiles.foreach{ case (name, bytes) =>
        os.write.over(tagCacheDir/s"$name.class", bytes)
      }

    }

    def compileCacheLoad(path: String, tag: Tag): Option[CompileCache] = {
      val tagCacheDir = {
        compileCacheDir/path.split('.').map(encode)/tag.code/tag.env/tag.classPathWhitelistHash
      }
      if(!os.exists(tagCacheDir)) None
      else for{
        (loadedTag, metadata) <- readJson[(Tag, Imports)](tagCacheDir/metadataFile)

        if tag == loadedTag
        classFiles <- loadClassFiles(tagCacheDir)
      } yield {
        CompileCache(classFiles, metadata)
      }
    }

    def loadClassFiles(cacheDir: os.Path): Option[ClassFiles] = {
      val classFiles = os.list(cacheDir).filter(_.ext == "class").toVector
      Try{
        val data = classFiles.map{ case file =>
          val className = (file relativeTo cacheDir).toString.stripSuffix(".class")
          (className, os.read.bytes(file))
        }
        data
      }.toOption
    }

    val ivyCache = new StableRef[IvyMap]{
      def apply() = {
        val json =
          try os.read(ivyCacheFile)
          catch{ case e: java.nio.file.NoSuchFileException => "[]" }

        val map =
          try upickle.default.read[IvyMap](json)
          catch{ case e: Exception => Map.empty }
        // Check that cached files exist
        map.filter(_._2.forall(str => Files.exists(Paths.get(str)))).asInstanceOf[IvyMap]
      }
      def update(map: IvyMap) = {
        os.write.over(ivyCacheFile, upickle.default.stream(map, indent = 4))
      }
    }

    def loadPredef = {
      try Some((os.read(predef), predef))
      catch { case e: java.nio.file.NoSuchFileException => Some(("", predef))}
    }
  }
}
