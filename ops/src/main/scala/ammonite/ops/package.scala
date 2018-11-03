package ammonite


package object ops extends Extensions {
  implicit val postfixOps = scala.language.postfixOps
  type ResourceNotFoundException = os.ResourceNotFoundException
  val ResourceNotFoundException = os.ResourceNotFoundException

  val PathError = os.PathError

  type Path = os.Path
  val Path = os.Path

  type RelPath = os.RelPath
  val RelPath = os.RelPath

  type FilePath = os.FilePath
  val FilePath = os.FilePath

  type BasePath = os.BasePath
  val BasePath = os.BasePath

  type ResourceRoot = os.ResourceRoot
  val ResourceRoot = os.ResourceRoot

  type FileType = os.FileType
  val FileType = os.FileType

  type PermSet = os.PermSet
  val PermSet = os.PermSet

  object stat extends Function1[Path, os.StatInfo]{
    def apply(s: Path, followLinks: Boolean = true) = os.stat(s, followLinks)
    def apply(s: Path) = os.stat(s)
    val full = os.stat.full
  }
  implicit def SymPath(s: Symbol): RelPath = StringPath(s.name)
  implicit def StringPath(s: String): RelPath = {
    BasePath.checkSegment(s)
    RelPath(s)
  }

  val exists = os.exists
  val read = os.read
  val write = os.write
  val rm = os.remove.all
  val mkdir = os.makeDir.all
  object ls extends Function1[Path, LsSeq]{
    def !(implicit arg: Path) = apply(arg)
    def apply(src: Path) = {
      LsSeq(src, os.list(src).map(_ relativeTo src).toArray.sorted:_*)
    }
    def iter(p: Path) = os.list.stream(p)
    object rec extends Function1[Path, LsSeq]{
      def apply(src: Path) =
        LsSeq(src, os.walk(src).map(_ relativeTo src).toArray.sorted:_*)
    }
  }


  object ln extends Function2[Path, Path, Unit]{
    def apply(src: Path, dest: Path) = os.hardlink(src, dest)
    val s = os.symlink
  }
  /**
   * The root of the filesystem
   */
  val root = os.root
  val empty = os.rel

  def resource(implicit resRoot: ResourceRoot = Thread.currentThread().getContextClassLoader) =
    os.resource

  /**
   * The user's home directory
   */
  val home = os.home

  val tmp = os.temp
  /**
   * The current working directory for this process.
   */
  val pwd = os.pwd

  val up = os.up

  /**
    * If you want to call subprocesses using [[%]] or [[%%]] and don't care
    * what working directory they use, import this via
    *
    * `import ammonite.ops.ImplicitWd._`
    *
    * To make them use the process's working directory for each subprocess
    */
  object ImplicitWd{
    implicit lazy val implicitCwd = ops.pwd
  }

  /**
    * Extractor to let you easily pattern match on [[ops.Path]]s. Lets you do
    *
    * {{{
    *   @ val base/segment/filename = pwd
    *   base: Path = Path(Vector("Users", "haoyi", "Dropbox (Personal)"))
    *   segment: String = "Workspace"
    *   filename: String = "Ammonite"
    * }}}
    *
    * To break apart a path and extract various pieces of it.
    */
  val / = os./

  /**
    * Lets you treat any path as a file, letting you access any property you'd
    * normally access through [[stat]]-ing it by [[stat]]-ing the file for you
    * when necessary.
    */
  implicit def fileData(p: Path): os.FullStatInfo = stat.full(p)

  /**
    * Used to spawn a subprocess interactively; any output gets printed to the
    * console and any input gets requested from the current console. Can be
    * used to run interactive subprocesses like `%vim`, `%python`,
    * `%ssh "www.google.com"` or `%sbt`.
    */
  val % = Shellout.%
  /**
    * Spawns a subprocess non-interactively, waiting for it to complete and
    * collecting all output into a [[CommandResult]] which exposes it in a
    * convenient form. Call via `%%('whoami).out.trim` or
    * `%%('git, 'commit, "-am", "Hello!").exitCode`
    */
  val %% = Shellout.%%
}
