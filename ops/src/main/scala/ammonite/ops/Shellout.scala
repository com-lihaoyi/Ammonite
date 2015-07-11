package ammonite.ops

import pprint.{PPrint, Config, PPrinter}

import scala.language.dynamics

object Shellout{

  def executeInteractive(executed: Command[_]) = {
    val process = new java.lang.ProcessBuilder()
    process.environment()
    process.directory(new java.io.File(executed.wd.toString))
    process
      .command(executed.cmd:_*)
      .inheritIO()
      .start()
      .waitFor()

  }
  def executeStream(executed: Command[_]) = {
    import scala.sys.process._
    val p = Process(executed.cmd, new java.io.File(executed.wd.toString))
    CommandResult(p.lineStream)
  }
}

/**
 * A staged sub-process command that has yet to be executed.
 */
class Command[T](val wd: Path,
                 val cmd: Vector[String],
                 execute: Command[_] => T) extends Dynamic {
  def extend(cmd2: Vector[String]) = new Command(wd, cmd ++ cmd2, execute)
  def selectDynamic(s: String) = execute(extend(Vector(s)))
  def applyDynamic(op: String)(args: Shellable*): T = {
    execute(this.extend(Vector(op) ++ args.map(_.s)))
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

