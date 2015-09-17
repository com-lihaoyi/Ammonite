package ammonite.ops

import pprint.{PPrint, PPrinter}

import scala.language.dynamics

object Shellout{

  def executeInteractive(wd: Path, cmd: Command[_]) = {
    val process = new java.lang.ProcessBuilder()
    import collection.JavaConversions._
    process.environment().putAll(cmd.envArgs)
    process.directory(new java.io.File(wd.toString))
    process
      .command(cmd.cmd:_*)
      .inheritIO()
      .start()
      .waitFor()

  }
  def executeStream(wd: Path, cmd: Command[_]) = {
    import scala.sys.process._
    val p = Process(cmd.cmd, new java.io.File(wd.toString), cmd.envArgs.toSeq:_*)

    CommandResult(p.lines)
  }
}

/**
 * A staged sub-process command that has yet to be executed.
 */
case class Command[T](cmd: Vector[String],
                      envArgs: Map[String, String],
                      execute: (Path, Command[_]) => T,
                      blankCallsOnly: Boolean) extends Dynamic {
  def extend(cmd2: Traversable[String], envArgs2: Traversable[(String, String)]) =
    new Command(cmd ++ cmd2, envArgs ++ envArgs2, execute, blankCallsOnly)
  def selectDynamic(name: String)(implicit wd: Path) = execute(wd, extend(Vector(name), Map()))
  def opArg(op: String) = {
    if (!blankCallsOnly) Vector(op)
    else {
      assert(op == "apply")
      Vector()
    }
  }
  def applyDynamic(op: String)(args: Shellable*)(implicit wd: Path): T = {
    execute(wd, this.extend(opArg(op) ++ args.flatMap(_.s), Map()))
  }
  def applyDynamicNamed(op: String)
                       (args: (String, Shellable)*)
                       (implicit wd: Path): T = {
    val (namedArgs, posArgs) = args.map{case (k, v) => (k, v.s)}.partition(_._1 != "")
    execute(wd, this.extend(opArg(op) ++ posArgs.flatMap(_._2),
      namedArgs.map{case (k, v) => (k, v.head)}))
  }
}

/**
 * Wrapper for the `Stream[String]` of lines returned by a subprocess
 * command, but with a better `PPrint` that makes it look like one
 * block of text for easy reading
 */
case class CommandResult(output: Stream[String]) extends Seq[String]{
  def iterator = output.iterator
  def apply(idx: Int) = output(idx)
  def length: Int = output.length
}

object CommandResult{
  implicit def commandResultRepr = PPrint(
    PPrinter[CommandResult]((x, c) =>
      x.output.iterator.flatMap(line =>
        Iterator("\n", c.colors.literalColor, line, c.colors.endColor)
      )
    )
  )
}

/**
 * An implicit wrapper defining the things that can
 * be "interpolated" directly into a subprocess call.
 */
case class Shellable(s: Seq[String])
object Shellable{
  implicit def StringShellable(s: String): Shellable = Shellable(Seq(s))
  implicit def SeqShellable(s: Seq[String]): Shellable = Shellable(s)
  implicit def SymbolShellable(s: Symbol): Shellable = Shellable(Seq(s.name))
  implicit def BasePathShellable(s: BasePath[_]): Shellable = Shellable(Seq(s.toString))
  implicit def NumericShellable[T: Numeric](s: T): Shellable = Shellable(Seq(s.toString))
}

/**
 * Dynamic shell command execution. This allows you to run commands which
 * are not provided by Ammonite, by shelling out to bash. e.g. try
 *
 * %ls
 * %ls "/"
 * %ps 'aux
 */
abstract class ShelloutRoots(prefix: Seq[String], blankCallsOnly: Boolean) {
  val % = new Command(prefix.toVector, Map.empty, Shellout.executeInteractive, blankCallsOnly)
  val %% = new Command(prefix.toVector, Map.empty, Shellout.executeStream, blankCallsOnly)
}