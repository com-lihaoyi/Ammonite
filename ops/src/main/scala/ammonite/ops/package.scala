package ammonite

import scala.collection.Seq

package object ops extends Extensions with RelPathStuff{
  /**
   * The root of the filesystem
   */
  val root = ops.Path.root

  /**
   * The user's home directory
   */
  val home = Path(System.getProperty("user.home"))

  /**
   * The current working directory for this process.
   */
  lazy val cwd = ops.Path(new java.io.File(""))

  implicit class Transformable1(p: java.nio.file.Path){
    def amm = {
      val s = p.toString

      if (s.startsWith("/")) ops.Path(s)
      else ops.RelPath(s)
    }
  }

  /**
   * Extractor to let you easily pattern match on [[ops.Path]]s
   */
  object /{

    def unapply[T <: BasePath[T]](p: T): Option[(T, String)] = {
      if (p.segments.length > 0)
        Some((p / up, p.last))
      else None
    }
  }


  implicit def fileData(p: Path) = stat.full(p)
  
  private val bashCompletionFunction = """__print_completions() { for ((i=0;i<${#COMPREPLY[*]};i++)); do echo ${COMPREPLY[i]}; done }"""
  implicit class BashInterpolator(val sc: StringContext) {
    
    object sh {
      def apply(args: String*) = %.execute(Seq("env", "bash", "-c", sc.s(args:_*)))
      
      def suggestions(args: String*): Seq[String] = {
        val compLine = sc.s(args:_*)
        if (compLine.isEmpty) Seq.empty
        else {
          val compWords = compLine.split(" ")
          val cmdName = compWords(0)
          val compCword = compWords.size - 1 + (if (compLine.endsWith(" ")) 1 else 0)
          val lastWord = if (compLine.endsWith(" ")) "" else compWords.last
          val penultimateWord = if (compLine.endsWith(" ")) compWords.last else compWords.init.lastOption.getOrElse("")
          
          /*got to source the appropriate file under /usr/share/bash-completion/completions
           to do that we will source all the files that contains the name of the command to try to aproximate to it*/
          val completionsToLoad = ls! root / 'usr / 'share / "bash-completion" / 'completions |? (_.name contains cmdName) | (f => s"source $f")
          
          val bashSourceLine = s"source /etc/bash_completion; [[ -r ~/.bash_completion ]] && echo yeah || echo boo" + (if (completionsToLoad.nonEmpty) s"; ${completionsToLoad.mkString("; ")}" else "")
          val bashLine = s"$bashSourceLine; complete -p"
          val availableCompletions = %.execute(Seq("env", "bash", "-c", bashLine))
          availableCompletions.filter(_.endsWith(" " + cmdName)).headOption match {
            case Some(registeredComp) =>
//              println("Registered comp: " + registeredComp)
              val function = registeredComp.split(" ").reverse.drop(1).head
              val getCompletionsBashCmd = s"""$bashSourceLine; $bashCompletionFunction; """ + 
              s"""COMP_WORDS=($compLine); COMP_LINE="$compLine"; COMP_CWORD=$compCword; COMP_POINT=${compLine.length + 1}; """ + 
              s"""$function '$cmdName' '$lastWord' '$penultimateWord'; """ + 
              s"__print_completions"
//              println(getCompletionsBashCmd)
              val possibilities = %.execute(Seq("env", "bash", "-c", getCompletionsBashCmd))
//              println(possibilities)
              possibilities.collect {case s if s.startsWith(lastWord) => s.stripPrefix(lastWord)}
            case _ => Seq.empty
          }
        }
      }
    }
  }
}
