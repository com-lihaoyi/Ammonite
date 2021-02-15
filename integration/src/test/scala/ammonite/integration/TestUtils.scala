package ammonite.integration

import ammonite.ops._
import ammonite.util.Util
import ImplicitWd._

/**
  * Created by haoyi on 6/4/16.
  */
object TestUtils {
  val scalaVersion = ammonite.compiler.CompilerBuilder.scalaVersion
  val isScala2 = scalaVersion.startsWith("2.")
  val javaVersion = scala.util.Properties.javaVersion
  val ammVersion = ammonite.Constants.version
  val executable = {
    val p = System.getenv("AMMONITE_ASSEMBLY")
    if (Util.windowsPlatform)
      Seq(p)
    else
      Seq("bash", p)
  }
  val intTestResources = pwd/'integration/'src/'test/'resources
  val replStandaloneResources = intTestResources/'ammonite/'integration
  val shellAmmoniteResources = pwd/'shell/'src/'main/'resources/'ammonite/'shell
  val emptyPrefdef = shellAmmoniteResources/"empty-predef.sc"
  val exampleBarePredef = shellAmmoniteResources/"example-predef-bare.sc"

  //we use an empty predef file here to isolate the tests from external forces.
  def execBase(name: RelPath,
               extraAmmArgs: Seq[String],
               home: os.Path,
               args: Seq[String],
               thin: Boolean,
               extraEnv: Iterable[(String, String)]) = {
    os.proc(
      executable,
      extraAmmArgs,
      if (thin) Seq("--thin") else Nil,
      "--no-remote-logging",
      "--home",
      home,
      replStandaloneResources / name,
      args
    ).call(
      env = Map("JAVA_OPTS" -> null) ++ extraEnv,
      stderr = os.Pipe
    )
  }
  def exec(name: RelPath, args: String*) =
    execBase(name, Nil, tmp.dir(), args, thin = true, Nil)
  def execWithEnv(env: Iterable[(String, String)], name: RelPath, args: String*) =
    execBase(name, Nil, tmp.dir(), args, thin = true, env)
  def execNonThin(name: RelPath, args: String*) =
    execBase(name, Nil, tmp.dir(), args, thin = false, Nil)
  def execWithHome(home: os.Path, name: RelPath, args: String*) =
    execBase(name, Nil, home, args, thin = true, Nil)
  def execSilent(name: RelPath, args: String*) =
    execBase(name, Seq("-s"), tmp.dir(), args, thin = true, Nil)

  /**
    *Counts number of non-overlapping occurrences of `subs` in `s`
    */
  def substrCount(s: String, subs: String, count: Int = 0, ptr: Int = 0): Int = {
    s.indexOf(subs, ptr) match{
      case -1 => count
      case x => substrCount(s, subs, count+1, x + subs.length)
    }
  }
}
