package ammonite.repl.tools

import ammonite.runtime.tools.browse.Strings
import ammonite.util.CodeColors
import sourcecode.Compat._

import scala.annotation.tailrec
import scala.language.experimental.macros

object source{


  def load(f: => Any): Location = macro loadMacro

  def loadMacro(c: Context)
               (f: c.Expr[Any]): c.Expr[Location] = {

    import c.universe._

    val res = breakUp(c)(f) match{
      case None =>
        q"${prefix(c)}.failLoudly(${prefix(c)}.loadObjectInfo($f))"
      case Some((classThingy, symbolName, lhs, returnClass, argClasses)) =>
        q"""
        ${prefix(c)}.failLoudly(
          ${prefix(c)}.loadObjectMemberInfo(
            $classThingy,
            $lhs,
            $symbolName,
            $returnClass,
            ..$argClasses
          )
        )
        """
    }

    c.Expr[Location](res)
  }

  def apply(f: => Any)
           (implicit pprinter: pprint.PPrinter,
            colors: CodeColors): Unit = macro applyMacro

  def apply(f: => Any, command: Int => Strings)
           (implicit pprinter: pprint.PPrinter,
            colors: CodeColors): Unit = macro applyCustomizeCommandMacro

  def applyMacro(c: Context)
                (f: c.Expr[Any])
                (pprinter: c.Expr[pprint.PPrinter],
                  colors: c.Expr[CodeColors]): c.Expr[Unit] = {
    import c.universe._
    val defaultBrowseExpr = c.Expr[Int => Strings](
      q"_root_.ammonite.repl.tools.SourceRuntime.browseSourceCommand"
    )

    applyCustomizeCommandMacro(c)(f, defaultBrowseExpr)(pprinter, colors)
  }

  def applyCustomizeCommandMacro(c: Context)
                                (f: c.Expr[Any], command: c.Expr[Int => Strings])
                                (pprinter: c.Expr[pprint.PPrinter],
                  colors: c.Expr[CodeColors]): c.Expr[Unit] = {
    import c.universe._
    c.Expr[Unit](
      breakUp(c)(f) match{
        case Some((classThingy, symbolName, lhs, returnClass, argClasses)) =>
          q"""
          ${prefix(c)}.browseObjectMember(
            $classThingy,
            $lhs,
            $symbolName,
            $pprinter,
            $colors,
            $command,
            $returnClass,
            ..$argClasses
          )
          """
        case None => q"${prefix(c)}.browseObject($f, $pprinter, $colors, $command)"
      }
    )
  }

  def prefix(c: Context) = {
    import c.universe._
    q"ammonite.repl.tools.SourceRuntime"
  }
  /**
    * Attempts to up an expression, into either a LHS + methodcall + rhs. We
    * then look for the source of the method. If it can't be split, we look for
    * the source of the class of the entire expression
    */
  def breakUp(c: Context)(f: c.Expr[Any]) = {
    import c.universe._
    // Break up the expression into it's constituent parts
    //
    // We don't use quasiquote pattern matching here, because we were seeing
    // weird behavior where the quasiquote q"{..$foo; $bar}" would match single
    // expressions not enclosed in blocks, and recursing on `bar` would result
    // in an infinite recursion. No such problem matching on the `Block` AST node.
    //
    // We keep the block wrapper to re-apply to the final expression later, because
    // sometimes (e.g. in the case of `new javassist.ClassPool().find _`) the LHS of
    // the last expr in the block ends up depending on the earlier statements
    @tailrec def rec(wrapper: Tree => Tree, x: Tree)
                    : Option[(Tree, Symbol, Tree => Tree)] = {
      x match {
        case Select(qualifier, selector) =>
          if (selector.toString == "<init>") None
          else if (qualifier.symbol != null && qualifier.symbol.isPackage) None
          else if (!x.symbol.isMethod) None
          else Some(qualifier, x.symbol, wrapper)
        case Apply(fun, args) => rec(wrapper, fun)

        case TypeApply(fun, targs) => rec(wrapper, fun)
        case Function(vparams, body) => rec(wrapper, body)
        case Block(stats, expr) => rec(Block(stats, _), expr)
        case _ => None
      }
    }

    for((lhs, symbol, wrapper) <- rec(identity(_), f.tree)) yield {

      val method = symbol.asMethod

      val argClasses =
        for(arg <- method.paramss.flatten)
        yield q"classOf[${arg.typeSignature.erasure}]"

      val staticJavaLhsClass = lhs.symbol != null && lhs.symbol.isStatic && lhs.symbol.isJava

      val ownerCls = symbol.owner.asClass

      val paramedOwnerCls = ownerCls.typeParams.length match{
        case 0 => ownerCls.thisPrefix.widen
        case n =>
          import compat._
          TypeRef(ownerCls.thisPrefix, ownerCls, List.fill(n)(typeOf[Nothing]))
      }

      val ownerTree =
        if (staticJavaLhsClass) Compat.companion(c)(ownerCls)
        else q"classOf[${paramedOwnerCls.erasure}]"

      Tuple5(
        ownerTree,
        symbol.name.toString,
        // static Java classes don't *have* a LHS value we can pass in to
        // do a runtime check, so leave it out and just do the compile-time check
        wrapper(if (staticJavaLhsClass) q"None" else q"Some($lhs)"),
        // We special case scala.Unit. This normally erases to BoxedUnit,
        // which is correct everywhere except method return types where it
        // has to erase to void (which is classOf[scala.Unit])
        if (method.returnType =:= typeOf[Unit]) q"classOf[scala.Unit]"
        else q"classOf[${method.returnType.erasure}]",
        argClasses
      )
    }
  }

}
