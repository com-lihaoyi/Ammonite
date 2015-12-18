package ammonite.repl

import java.nio.file.{Paths, Files}

import Util.{IvyMap, CompileCache}
import scala.collection.mutable

class MemoryStorage extends Storage{
  var predef = ""
  def loadPredef = predef

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

  var compileCache: mutable.Map[String,CompileCache] = mutable.Map.empty
  def compileCacheSave(tag: String, data: CompileCache): Unit = compileCache(tag) = data
  def compileCacheLoad(tag: String) = compileCache.get(tag)

  val sessionClassFilesDir = null //Files.createTempDirectory(Paths.get("/tmp/"), null)
}
