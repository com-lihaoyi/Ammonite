package ammonite.ops

import pprint.{PPrint, PPrinter}

import scala.language.dynamics

object Shellout{

  def executeInteractive(wd: Path, executed: Command[_]) = {
    val process = new java.lang.ProcessBuilder()
    process.environment()
    process.directory(new java.io.File(wd.toString))
    process
      .command(executed.cmd:_*)
      .inheritIO()
      .start()
      .waitFor()

  }
  def executeStream(wd: Path, executed: Command[_]) = {
    import scala.sys.process._
    val p = Process(executed.cmd, new java.io.File(wd.toString))
    CommandResult(p.lines)
  }
}

/**
 * A staged sub-process command that has yet to be executed.
 */
class Command[T](val cmd: Vector[String],
                 val envArgs: Map[String, String],
                 execute: (Path, Command[_]) => T) extends Dynamic {
  def extend(cmd2: Vector[String]) = new Command(cmd ++ cmd2, envArgs, execute)
  def selectDynamic(name: String)(implicit wd: Path) = execute(wd, extend(Vector(name)))
  def applyDynamic(op: String)(args: Shellable*)(implicit wd: Path): T = {
    execute(wd, this.extend(Vector(op) ++ args.map(_.s)))
  }
  def applyDynamicNamed(name: String)(args: (String, Shellable)*): Command[T] = {
    assert(
      name == "apply",
      s"""% or %% can only be called directly with keyword args,
         |e.g. %(X=foo, Y=bar). You cannot call it with keyword
         |arguments and a method named [$name]""".stripMargin
    )
    val (blankIndices, blankArgs) =
      args.zipWithIndex
          .collect{ case (("", y), i) => (y.s, i)}
          .unzip

    assert(
      blankArgs.isEmpty,
      s"""When % or %% is called with named args, e.g. %(X=foo, Y=bar),
         |every argument must be written with a name. Arguments
         |[${blankIndices.mkString(", ")}] with values
         |${blankArgs.mkString(", ")} did not have names""".stripMargin
    )

    new Command(
      cmd,
      envArgs ++ args.map{case (x, y) => (x, y.s)},
      execute
    )
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
case class Shellable(s: String)
object Shellable{
  implicit def StringShellable(s: String): Shellable = Shellable(s)
  implicit def SymbolShellable(s: Symbol): Shellable = Shellable(s.name)
  implicit def BasePathShellable(s: BasePath[_]): Shellable = Shellable(s.toString)
  implicit def NumericShellable[T: Numeric](s: T): Shellable = Shellable(s.toString)
}


/**
 * Dynamic shell command execution. This allows you to run commands which
 * are not provided by Ammonite, by shelling out to bash. e.g. try
 *
 * %ls
 * %ls "/"
 * %ps 'aux
 */
object % extends Command(Vector.empty, Map.empty, Shellout.executeInteractive)
object %% extends Command(Vector.empty, Map.empty, Shellout.executeStream)