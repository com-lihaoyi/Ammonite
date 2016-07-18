package ammonite.frontend

import org.apache.ivy.plugins.repository.file.FileRepository
import org.apache.ivy.plugins.resolver.{FileSystemResolver, IBiblioResolver, RepositoryResolver}

/**
  * A thin wrapper around [[RepositoryResolver]], which wraps them and provides
  * hashability in order to set the cache tags. This lets us invalidate the ivy
  * resolution cache if the set of resolvers changes
  */
sealed trait Resolver{
  def apply(): RepositoryResolver
}
object Resolver{
  case class File(name: String, root: String, pattern: String, m2: Boolean) extends Resolver{
    def apply() = {
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
  }
  case class Http(name: String, root: String, pattern: String, m2: Boolean) extends Resolver{
    def apply() = {
      val res = new IBiblioResolver()
      res.setUsepoms(true)
      res.setM2compatible(m2)
      res.setName(name)
      res.setRoot(root)
      res.setPattern(pattern)
      res
    }
  }
}
