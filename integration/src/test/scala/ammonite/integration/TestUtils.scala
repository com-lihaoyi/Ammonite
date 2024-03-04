package ammonite.integration

import ammonite.util.Util

/**
 * Created by haoyi on 6/4/16.
 */
object TestUtils {
  val scalaVersion = ammonite.compiler.CompilerBuilder.scalaVersion
  val isScala2 = scalaVersion.startsWith("2.")
  val javaVersion = scala.util.Properties.javaVersion
  val ammVersion = ammonite.Constants.version
  val ammAssembly = System.getenv("AMMONITE_ASSEMBLY")
  val executable =
    if (Util.windowsPlatform)
      Seq(ammAssembly)
    else
      Seq("bash", ammAssembly)
  val intTestResources = os.pwd / "integration" / "src" / "test" / "resources"
  val replStandaloneResources = intTestResources / "ammonite" / "integration"
  val shellAmmoniteResources =
    os.pwd / "shell" / "src" / "main" / "resources" / "ammonite" / "shell"

  // we use an empty predef file here to isolate the tests from external forces.
  def execBase(
      name: os.RelPath,
      extraAmmArgs: Seq[String],
      home: os.Path,
      args: Seq[String],
      thin: Boolean,
      extraEnv: Iterable[(String, String)]
  ) = {
    os.proc(
      executable,
      extraAmmArgs,
      if (thin) Seq("--thin") else Nil,
      "--home",
      home,
      replStandaloneResources / name,
      args
    ).call(
      env = Map("JAVA_OPTS" -> null) ++ extraEnv,
      stderr = os.Pipe
    )
  }
  def exec(name: os.RelPath, args: String*) =
    execBase(name, Nil, os.temp.dir(), args, thin = true, Nil)
  def execWithEnv(env: Iterable[(String, String)], name: os.RelPath, args: String*) =
    execBase(name, Nil, os.temp.dir(), args, thin = true, env)
  def execNonThin(name: os.RelPath, args: String*) =
    execBase(name, Nil, os.temp.dir(), args, thin = false, Nil)
  def execWithHome(home: os.Path, name: os.RelPath, args: String*) =
    execBase(name, Nil, home, args, thin = true, Nil)
  def execSilent(name: os.RelPath, args: String*) =
    execBase(name, Seq("-s"), os.temp.dir(), args, thin = true, Nil)

  /**
   * Counts number of non-overlapping occurrences of `subs` in `s`
   */
  def substrCount(s: String, subs: String, count: Int = 0, ptr: Int = 0): Int = {
    s.indexOf(subs, ptr) match {
      case -1 => count
      case x => substrCount(s, subs, count + 1, x + subs.length)
    }
  }

  def containsLines(output: String, expected: String): Boolean =
    containsLines(output.linesIterator.toList, expected.linesIterator.toList)

  @annotation.tailrec
  def containsLines(output: List[String], expected: List[String]): Boolean =
    expected match {
      case Nil => true
      case h :: t =>
        output match {
          case Nil => false
          case h0 :: t0 =>
            val remaining = if (h0.contains(h)) t else expected
            containsLines(t0, remaining)
        }
    }
}
