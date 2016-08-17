package ammonite.main
import acyclic.file
import sourcecode.Compat.Context

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
/**
  * More or less a minimal version of Autowire's Server that lets you generate
  * a set of "routes" from the methods defined in an object, and call them
  * using passing in name/args/kwargs via Java reflection, without having to
  * generate/compile code or use Scala reflection. This saves us spinning up
  * the Scala compiler and greatly reduces the startup time of cached scripts.
  */
object Router{
  class doc(s: String) extends StaticAnnotation
  class main extends StaticAnnotation
  def generateRoutes[T](t: T): Seq[Router.EntryPoint] = macro generateRoutesImpl[T]
  def generateRoutesImpl[T: c.WeakTypeTag](c: Context)(t: c.Expr[T]): c.Expr[Seq[EntryPoint]] = {
    import c.universe._
    val r = new Router(c)
    val allRoutes = r.getAllRoutesForClass(
      weakTypeOf[T].asInstanceOf[r.c.Type],
      t.tree.asInstanceOf[r.c.Tree]
    ).asInstanceOf[Iterable[c.Tree]]

    val res = q"_root_.scala.Seq(..$allRoutes)"
//    println(res)
    c.Expr[Seq[EntryPoint]](res)
  }

  /**
    * Models what is known by the router about a single argument: that it has
    * a [[name]], a human-readable [[typeString]] describing what the type is
    * (just for logging and reading, not a replacement for a `TypeTag`) and
    * possible a function that can compute its default value
    */
  case class ArgSig(name: String,
                    typeString: String,
                    doc: Option[String],
                    default: Option[() => Any])

  /**
    * What is known about a single endpoint for our routes. It has a [[name]],
    * [[argSignatures]] for each argument, and a macro-generated [[invoke0]]
    * that performs all the necessary argument parsing and de-serialization.
    *
    * Realistically, you will probably spend most of your time calling [[invoke]]
    * instead, which provides a nicer API to call it that mimmicks the API of
    * calling a Scala method.
    */
  case class EntryPoint(name: String,
                        argSignatures: Seq[ArgSig],
                        varargs: Boolean,
                        invoke0: (Map[String, String], Seq[String]) => Result[Any]){
    def invoke(args: Seq[String], kwargs: Seq[(String, String)]): Result[Any] = {
      val (usedArgs, leftoverArgs) = args.splitAt(argSignatures.length)
      if (leftoverArgs.nonEmpty && !varargs) Result.Error.TooManyArguments(leftoverArgs)
      else {
        val implicitlyNamedArgs = argSignatures.map(_.name).zip(usedArgs).toMap
        val redundantKeys =
          (implicitlyNamedArgs.keys.toSeq ++ kwargs.map(_._1))
            .groupBy(x=>x)
            .filter(_._2.size > 1)

        if(redundantKeys.nonEmpty) {
          Result.Error.RedundantArguments(redundantKeys.keysIterator.toSeq)
        } else {
          try invoke0(implicitlyNamedArgs ++ kwargs, leftoverArgs)
          catch{case e: Throwable =>
            Result.Error.Exception(e)
          }
        }
      }
    }
  }

  def read[T](dict: Map[String, String],
              default: => Option[Any],
              arg: ArgSig,
              thunk: String => T,
              extras: Option[Seq[String]]): FailMaybe = {
    dict.get(arg.name) match{
      case None =>
        try default match{
          case None => Left(Seq(Result.ParamError.Missing(arg)))
          case Some(v) => Right(v)
        } catch {case e => Left(Seq(Result.ParamError.DefaultFailed(arg, e))) }

      case Some(x) =>

        extras match{
          case None =>
            try Right(thunk(x))
            catch {case e => Left(Seq(Result.ParamError.Invalid(arg, x, e))) }

          case Some(extraItems) =>
            val attempts: Seq[Either[Result.ParamError.Invalid, T]] = (x +: extraItems).map{ item =>
              try Right(thunk(item))
              catch {case e => Left(Result.ParamError.Invalid(arg, item, e)) }
            }

            val bad = attempts.collect{ case Left(x) => x}
            if (bad.nonEmpty) Left(bad)
            else {
              val good = Right(attempts.collect{case Right(x) => x})
              good
            }
        }

    }
  }

  /**
    * Represents what comes out of an attempt to invoke an [[EntryPoint]].
    * Could succeed with a value, but could fail in many different ways.
    */
  sealed trait Result[+T]
  object Result{

    /**
      * Invoking the [[EntryPoint]] was totally successful, and returned a
      * result
      */
    case class Success[T](value: T) extends Result[T]

    /**
      * Invoking the [[EntryPoint]] was not successful
      */
    sealed trait Error extends Result[Nothing]
    object Error{

      /**
        * Invoking the [[EntryPoint]] failed with an exception while executing
        * code within it.
        */
      case class Exception(t: Throwable) extends Error
      /**
        * Invoking the [[EntryPoint]] failed as there were too many positional
        * arguments passed in, more than what is expected by the [[EntryPoint]]
        */
      case class TooManyArguments(values: Seq[String]) extends Error
      /**
        * Invoking the [[EntryPoint]] failed as the same argument was passed
        * in more than once; possibly as a keyword-argument that was repeated,
        * or a keyword-argument and positional-argument that both resolve to
        * the same arg
        */
      case class RedundantArguments(names: Seq[String]) extends Error

      /**
        * Invoking the [[EntryPoint]] failed because there were problems
        * deserializing/parsing individual arguments
        */
      case class InvalidArguments(values: Seq[ParamError]) extends Error
    }

    /**
      * What could go wrong when trying to parse an individual parameter to
      * an [[EntryPoint]]?
      */
    sealed trait ParamError
    object ParamError {
      /**
        * Some parameter was missing from the input.
        */
      case class Missing(arg: ArgSig) extends ParamError
      /**
        * Something went wrong trying to de-serialize the input parameter;
        * the thrown exception is stored in [[ex]]
        */
      case class Invalid(arg: ArgSig, value: String, ex: Throwable) extends ParamError
      /**
        * Something went wrong trying to evaluate the default value
        * for this input parameter
        */
      case class DefaultFailed(arg: ArgSig, ex: Throwable) extends ParamError
    }
  }


  type FailMaybe = Either[Seq[Result.ParamError], Any]
  type FailAll = Either[Seq[Result.ParamError], Seq[Any]]

  def validate(args: Seq[FailMaybe]): Result[Seq[Any]] = {
    val lefts = args.collect{case Left(x) => x}.flatten
    if (lefts.nonEmpty) Result.Error.InvalidArguments(lefts)
    else Result.Success(args.collect{case Right(x) => x})
  }
}
class Router [C <: Context](val c: C) {
  import c.universe._
  def getValsOrMeths(curCls: Type): Iterable[MethodSymbol] = {
    def isAMemberOfAnyRef(member: Symbol) =
      weakTypeOf[AnyRef].members.exists(_.name == member.name)
    val extractableMembers = for {
      member <- curCls.declarations
      if !isAMemberOfAnyRef(member)
      if !member.isSynthetic
      if member.isPublic
      if member.isTerm
      memTerm = member.asTerm
      if memTerm.isMethod
    } yield memTerm.asMethod
    extractableMembers flatMap { case memTerm =>
      if (memTerm.isSetter || memTerm.isConstructor || memTerm.isGetter) Nil
      else Seq(memTerm)

    }
  }

  def extractMethod(meth: MethodSymbol,
                    curCls: c.universe.Type,
                    target: c.Tree): c.universe.Tree = {
    val flattenedArgLists = meth.paramss.flatten
    def hasDefault(i: Int) = {
      val defaultName = s"${meth.name}$$default$$${i + 1}"
      if (curCls.members.exists(_.name.toString == defaultName)) {
        Some(defaultName)
      } else {
        None
      }
    }
    val argListSymbol = q"${c.fresh[TermName]("argsList")}"
    val defaults = for ((arg, i) <- flattenedArgLists.zipWithIndex) yield {
      hasDefault(i).map(defaultName => q"() => $target.${newTermName(defaultName)}")
    }

    def unwrapVarargType(arg: Symbol) = {
      val vararg = arg.typeSignature.typeSymbol == definitions.RepeatedParamClass
      val unwrappedType =
        if (!vararg) arg.typeSignature
        else arg.typeSignature.asInstanceOf[TypeRef].args(0)

      (vararg, unwrappedType)
    }
    val readArgSigs = for(
      ((arg, defaultOpt), i) <- flattenedArgLists.zip(defaults).zipWithIndex
    ) yield {

      val (vararg, unwrappedType) = unwrapVarargType(arg)

      val default =
        if (vararg) q"scala.Some(scala.Nil)"
        else defaultOpt match {
          case Some(defaultExpr) => q"scala.Some($defaultExpr())"
          case None => q"scala.None"
        }

      val docs = for{
        doc <- arg.annotations.find(_.tpe =:= typeOf[Router.doc])
        if doc.scalaArgs.head.isInstanceOf[Literal]
        l =  doc.scalaArgs.head.asInstanceOf[Literal]
        if l.value.value.isInstanceOf[String]
      } yield l.value.value.asInstanceOf[String]

      val docTree = docs match{
        case None => q"scala.None"
        case Some(s) => q"scala.Some($s)"
      }
      val argSig = q"""
        ammonite.main.Router.ArgSig(
          ${arg.name.toString},
          ${arg.typeSignature.toString},
          $docTree,
          $defaultOpt
        )
      """

      val extraArg = if(vararg) q"scala.Some(extras)" else q"None"
      val reader = q"""
      ammonite.main.Router.read[$unwrappedType](
        $argListSymbol,
        $default,
        $argSig,
        implicitly[scopt.Read[$unwrappedType]].reads(_),
        $extraArg
      )
      """
      (reader, argSig, vararg)
    }

    val (readArgs, argSigs, varargs) = readArgSigs.unzip3
    val (argNames, argNameCasts) = flattenedArgLists.map { arg =>
      val (vararg, unwrappedType) = unwrapVarargType(arg)
      (
        pq"${arg.name.toTermName}",
        if (!vararg) q"${arg.name.toTermName}.asInstanceOf[$unwrappedType]"
        else q"${arg.name.toTermName}.asInstanceOf[Seq[$unwrappedType]]: _*"

        )
    }.unzip

    q"""
    ammonite.main.Router.EntryPoint(
      ${meth.name.toString},
      scala.Seq(..$argSigs),
      ${varargs.contains(true)},
      ($argListSymbol: Map[String, String], extras: Seq[String]) =>
        ammonite.main.Router.validate(Seq(..$readArgs)) match{
          case ammonite.main.Router.Result.Success(List(..$argNames)) =>
            ammonite.main.Router.Result.Success($target.${meth.name.toTermName}(..$argNameCasts))
          case x => x
        }
    )
    """
  }

  def getAllRoutesForClass(curCls: Type, target: c.Tree): Iterable[c.universe.Tree] = for{
    t <- getValsOrMeths(curCls)
    if t.annotations.exists(_.tpe =:= typeOf[Router.main])
  } yield extractMethod(t, curCls, target)
}

