package ammonite.interp

import ammonite.frontend.Session
import ammonite.util._

import scala.reflect.io.VirtualDirectory

/**
 * Evaluates already-compiled Bytecode.
  *
  * Deals with all the munging of `Classloader`s, `Class[_]` objects,
  * and `Array[Byte]`s representing class files, and reflection necessary
  * to take the already-compile Scala bytecode and execute it in our process.
 */
trait Evaluator{
  def loadClass(wrapperName: String, classFiles: ClassFiles): Res[Class[_]]
  def evalMain(cls: Class[_]): Any
  def getCurrentLine: String
  def update(newImports: Imports): Unit

  def processLine(classFiles: ClassFiles,
                  newImports: Imports,
                  printer: Printer,
                  fileName: String,
                  indexedWrapperName: Name): Res[Evaluated]

  def processScriptBlock(cls: Class[_],
                         newImports: Imports,
                         wrapperName: Name,
                         pkgName: Seq[Name],
                         tag: String): Res[Evaluated]

  def sess: Session

  def evalCachedClassFiles(cachedData: Seq[ClassFiles],
                           pkg: String,
                           wrapper: String,
                           dynamicClasspath: VirtualDirectory,
                           classFilesList: Seq[String]): Res[Seq[_]]

}
