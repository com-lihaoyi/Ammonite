package ammonite.interp

import scala.reflect.io.FileZipArchive
import scala.tools.nsc.Settings
import scala.tools.nsc.classpath.ZipAndJarClassPathFactory

abstract class ExtraCompilerCompatibility {

  def createZipJarFactory(arc: FileZipArchive, settings: Settings) = {
    ZipAndJarClassPathFactory.create(arc, settings, new scala.tools.nsc.CloseableRegistry())
  }

}
