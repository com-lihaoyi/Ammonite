package dotty.ammonite.compiler

import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Names.{Name, termName}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.interactive.Completion
import dotty.tools.dotc.util.Chars.{isOperatorPart, isScalaLetter}

trait AmmCompletionExtras {

  private val bslash = '\\'
  private val isDot = (x: Char) => x == '.'
  private val brackets = List('[',']','(',')','{','}')

  private def label(name: Name): String = {

    def maybeQuote(name: Name, recurse: Boolean): String =
      if (recurse && name.isTermName)
        name.asTermName.qualToString(maybeQuote(_, true), maybeQuote(_, false))
      // initially adapted from
      // https://github.com/scala/scala/blob/decbd53f1bde4600c8ff860f30a79f028a8e431d/
      //   src/reflect/scala/reflect/internal/Printers.scala#L573-L584
      else if (name == nme.CONSTRUCTOR) "this"
      else {
        val decName = name.decode.toString
        val hasSpecialChar = decName.exists { ch =>
          brackets.contains(ch) || ch.isWhitespace || isDot(ch)
        }
        def isOperatorLike = (name.isOperatorName || decName.exists(isOperatorPart)) &&
          decName.exists(isScalaLetter) &&
          !decName.contains(bslash)
        lazy val term = name.toTermName

        val needsBackTicks = hasSpecialChar ||
          isOperatorLike ||
          nme.keywords(term) && term != nme.USCOREkw

        if (needsBackTicks) s"`$decName`"
        else decName
      }

    maybeQuote(name, true)
  }


  def maybeBackticked(input: String, hasBackTick: Boolean): Completion =
    Completion(label(termName(input)), "", Nil)
  def backtick(completion: Completion): Completion =
    if (completion.label.startsWith("`")) completion
    else completion.copy(label = label(termName(completion.label)))

}
