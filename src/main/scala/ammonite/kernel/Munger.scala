package ammonite.kernel

import ammonite.util._
import ammonite.util.Util.{newLine, normalizeNewlines}
import scala.tools.nsc.{Global => G}
import collection.mutable

import scalaz.{Name => _, _}
import Scalaz._
import Validation.FlatMap._
import kernel.generatedMain

case class MungedOutput(code: String, prefixCharLength: Int)

/** Munges input statements into a form that can be fed into scalac
  */
object Munger {

  private case class Transform(code: String, resIden: Option[String])

  private type DCT = (String, String, G#Tree) => Option[Transform]

  def apply(parse: => String => ValidationNel[LogError, Seq[G#Tree]],
            stmts: NonEmptyList[String],
            resultIndex: String,
            pkgName: Seq[Name],
            indexedWrapperName: Name,
            imports: Imports): ValidationNel[LogError, MungedOutput] = {

    // type signatures are added below for documentation

    val decls: List[DCT] = {

      def DefProc(definitionLabel: String)(cond: PartialFunction[G#Tree, G#Name]): DCT =
        (code: String, name: String, tree: G#Tree) =>
          cond.lift(tree).map { name =>
            Transform(code, None)
        }

      def Processor(cond: PartialFunction[(String, String, G#Tree), Transform]): DCT = {
        (code: String, name: String, tree: G#Tree) =>
          cond.lift((name, code, tree))
      }

      val ObjectDef = DefProc("object") {
        case m: G#ModuleDef => m.name
      }

      val ClassDef = DefProc("class") {
        case m: G#ClassDef if !m.mods.isTrait => m.name
      }

      val TraitDef = DefProc("trait") {
        case m: G#ClassDef if m.mods.isTrait => m.name
      }

      val DefDef = DefProc("function") {
        case m: G#DefDef => m.name
      }

      val TypeDef = DefProc("type") {
        case m: G#TypeDef => m.name
      }

      val PatVarDef = Processor {
        case (name, code, t: G#ValDef) => Transform(code, None)
      }

      val Import = Processor {
        case (name, code, tree: G#Import) => Transform(code, None)
      }

      val Expr = Processor {
        //Expressions are lifted to anon function applications so they will be JITed
        case (name, code, tree) => Transform(s"private val $name = $code", Some(name))
      }

      List(
        ObjectDef,
        ClassDef,
        TraitDef,
        DefDef,
        TypeDef,
        PatVarDef,
        Import,
        Expr
      )
    }

    val composed: String => ValidationNel[LogError, (Seq[G#Tree], String)] = x => parse(x) map (y => (y, x))

    val parsed: ValidationNel[LogError, NonEmptyList[(Seq[G#Tree], String)]] = stmts.traverseU(composed)

    def declParser(inp: ((Seq[G#Tree], String), Int)): ValidationNel[LogError, Transform] = inp match {
      case ((trees, code), i) =>
        def handleTree(t: G#Tree): ValidationNel[LogError, Transform] = {
          val parsedDecls: List[Transform] = decls flatMap (x => x(code, "res" + resultIndex + "_" + i, t))
          parsedDecls match {
            case h :: t => Success(h)
            case Nil => Failure(NonEmptyList(LogError(s"Dont know how to handle $code")))
          }
        }
        trees match {
          case Seq(h) => handleTree(h)
          case _ if trees.nonEmpty && trees.forall(_.isInstanceOf[G#Import]) => handleTree(trees.head)
          case _ =>
            val filteredSeq = trees filter (_.isInstanceOf[G#ValDef])
            filteredSeq.toList.traverseU(handleTree).map { transforms =>
              transforms.lastOption match {
                case Some(Transform(_, resIden)) => Transform(code, resIden)
                case None => Transform(code, None)
              }
            }
        }
    }

    val declTraversed: ValidationNel[LogError, NonEmptyList[Transform]] =
      parsed.map(_.zipWithIndex).flatMap(_.traverseU(declParser))

    val expandedCode: ValidationNel[LogError, Transform] = declTraversed map {
      case NonEmptyList(h, tl) =>
        tl.foldLeft(h) {
          case (acc, v) => Transform(acc.code ++ v.code, v.resIden)
        }
    }

    expandedCode map {
      case Transform(code, resIden) =>
        // can't use strip Margin below because holier-than-thou libraries like shapeless and scalaz use weird
        // characters for identifiers

        val topWrapper = normalizeNewlines(s"""
           package ${pkgName.map(_.backticked).mkString(".")}
           ${importBlock(imports)}
           object ${indexedWrapperName.backticked}{\n""")

        val previousIden = resIden match {
          case None => s"()"
          case Some(iden) => iden
        }

        val bottomWrapper = normalizeNewlines(s"""
          def $generatedMain = { $previousIden }
          override def toString = "${indexedWrapperName.raw}"
          }""")

        val importsLen = topWrapper.length

        MungedOutput(topWrapper + code + bottomWrapper, importsLen)
    }

  }

  def importBlock(importData: Imports) = {
    // Group the remaining imports into sliding groups according to their
    // prefix, while still maintaining their ordering
    val grouped = mutable.Buffer[mutable.Buffer[ImportData]]()
    for (data <- importData.value) {
      if (grouped.isEmpty) grouped.append(mutable.Buffer(data))
      else {
        val last = grouped.last.last

        // Start a new import if we're importing from somewhere else, or
        // we're importing the same thing from the same place but aliasing
        // it to a different name, since you can't import the same thing
        // twice in a single import statement
        val startNewImport =
          last.prefix != data.prefix || grouped.last.exists(_.fromName == data.fromName)

        if (startNewImport) grouped.append(mutable.Buffer(data))
        else grouped.last.append(data)
      }
    }
    // Stringify everything
    val out = for (group <- grouped) yield {
      val printedGroup = for (item <- group) yield {
        if (item.fromName == item.toName) item.fromName.backticked
        else s"${item.fromName.backticked} => ${item.toName.backticked}"
      }
      val pkgString = group.head.prefix.map(_.backticked).mkString(".")
      "import " + pkgString + s".{$newLine  " +
        printedGroup.mkString(s",$newLine  ") + s"$newLine}$newLine"
    }
    out.mkString
  }

}
