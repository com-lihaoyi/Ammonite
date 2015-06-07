package ammonite.ops

import ammonite.pprint.{PPrint, Config, PPrinter}

import scala.language.dynamics

/**
 * Dynamic shell command execution. This allows you to run commands which
 * are not provided by Ammonite, by shelling out to bash. e.g. try
 *
 * %ls
 * %ls "/"
 * %ps "aux"
 */
object % extends %(Vector.empty)
case class %(cmd: Vector[String]) extends Dynamic with CommandBuilder[Int, %]{
  def extend(cmd2: Vector[String]) = new %(cmd ++ cmd2)
  def execute() = {
    new java.lang.ProcessBuilder().command(cmd:_*).inheritIO().start().waitFor()
    // This should work some day too
    //    %git                    %.git
    //    %git %diff              %.git(%).diff
    //    %git "clean"            %.git("clean")
    //    %git("clean", "-fdx")   %.git("clean", "-fdx")
    //    %git %clean %`-fdx`     %.git(%).clean(%).`-fdx`
    //    %git %clean "-fdx"      %.git(%).clean "-fdx"
  }
}

case class CommandResult(output: Stream[String]) extends Seq[String]{
  def iterator = output.iterator
  def apply(idx: Int) = output(idx)
  def length: Int = output.length
}

object CommandResult{
  implicit def commandResultRepr(implicit c: Config) = new PPrint(
    PPrinter[CommandResult]((x, c) =>
      x.output.iterator.flatMap(line =>
        Iterator("\n", c.color.literal(line))
      )
    ),
    c
  )
}

object %% extends %%(Vector.empty)
case class %%(cmd: Vector[String]) extends Dynamic with CommandBuilder[CommandResult, %%]{
  def extend(cmd2: Vector[String]) = new %%(cmd ++ cmd2)
  def execute() = {
    import scala.sys.process._
    CommandResult(cmd.lineStream)
  }
}


trait CommandBuilder[R, B <: CommandBuilder[R, B]]{
  def selectDynamic(s: String) = extend(Vector(s)).execute()
  def applyDynamic[T, V](op: String)(args: T*)(implicit ce: CommandExtender[R, B, T, V]): V = {
    ce.extend(this, op, args)
  }
  def extend(cmd2: Vector[String]): B
  def execute(): R
}

trait CommandExtender[R, B <: CommandBuilder[R, B], T, V]{
  def extend(cmd: CommandBuilder[R, B], op: String, args: Seq[T]): V
}

object CommandExtender{
  /**
   * You can call a command with strings, e.g.
   *
   * %git("reset", "--hard", "head")
   *
   * And we'll execute it as
   *
   * Seq("git", "reset", "--hard", "head")
   */
  implicit def Str[R, B <: CommandBuilder[R, B]] = new CommandExtender[R, B, String, R]{
    def extend(cmd: CommandBuilder[R, B], op: String, args: Seq[String]) =
      cmd.extend(Vector(op) ++ args).execute()
  }


  /**
   * You can always interleave a command e.g.
   *
   * %git %diff or %.git(%).diff
   *
   * And we'll treat it as executing the command
   *
   * Seq("git", "diff")
   *
   * immediately and returning the result
   */
  implicit def Chain[R, B <: CommandBuilder[R, B]] = new CommandExtender[R, B, %.type, B]{
    def extend(cmd: CommandBuilder[R, B], op: String, args: Seq[%.type]) =
      cmd.extend(Vector(op))
  }
}
