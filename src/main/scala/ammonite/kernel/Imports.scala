package ammonite.kernel

import collection.mutable

/**
  * Represents the importing of a single name in the Ammonite REPL, of the
  * form
  *
  * {{{
  * import $prefix.{$fromName => $toName}
  * }}}
  *
  * All imports are reduced to this form; `import $prefix.$name` is results in
  * the `fromName` and `toName` being the same, while `import $prefix._` or
  * `import $prefix.{foo, bar, baz}` are split into multiple distinct
  * [[ImportData]] objects.
  *
  * Note that imports can be of one of three distinct `ImportType`s: importing
  * a type, a term, or both. This lets us properly deal with shadowing correctly
  * if we import the type and term of the same name from different places
  */
private[kernel] case class ImportData(fromName: Name,
                                      toName: Name,
                                      prefix: Seq[Name],
                                      importType: ImportData.ImportType)

private[kernel] object ImportData {

  sealed abstract class ImportType(name: String)

  case object Type extends ImportType("Type")

  case object Term extends ImportType("Term")

  case object TermType extends ImportType("TermType")
}

/**
  * Represents the imports that occur before a piece of user code in the
  * Ammonite REPL. It's basically a `Seq[ImportData]`, except we really want
  * it to be always in a "canonical" form without shadowed/duplicate imports.
  *
  * Thus we only expose an `apply` method which performs this de-duplication,
  * and a `++` operator that combines two sets of imports while performing
  * de-duplication.
  */
private[kernel] class Imports private (val value: Seq[ImportData]) {

  def ++(others: Imports): Imports = Imports(this.value, others.value)

  override def toString(): String = s"Imports(${value.toString})"
}

private[kernel] object Imports {

  /**
    * Constructs an `Imports` object from one or more loose sequence of imports
    *
    * Figures out which imports will get stomped over by future imports
    * before they get used, and just ignore those.
    */
  def apply(importss: Seq[ImportData]*): Imports = {
    // We iterate over the combined reversed imports, keeping track of the
    // things that will-be-stomped-over-in-the-non-reversed-world in a map.
    // If an import's target destination will get stomped over we ignore it
    //
    // At the end of the day we re-reverse the trimmed list and return it.
    val importData = importss.flatten
    val stompedTypes = mutable.Set.empty[Name]
    val stompedTerms = mutable.Set.empty[Name]
    val out = mutable.Buffer.empty[ImportData]
    for (data <- importData.reverseIterator) {
      val stomped = (data.importType: @unchecked) match {
        case ImportData.Term => Seq(stompedTerms)
        case ImportData.Type => Seq(stompedTypes)
        case ImportData.TermType => Seq(stompedTerms, stompedTypes)
      }
      if (!stomped.exists(_(data.toName))) {
        out.append(data)
        stomped.foreach(_.add(data.toName))
        data.prefix.headOption.foreach(stompedTerms.remove)
      }
    }
    new Imports(out.reverse)
  }
}

private[kernel] case class ImportTree(prefix: Seq[String],
                                      mappings: Option[ImportTree.ImportMapping],
                                      start: Int,
                                      end: Int)

private[kernel] object ImportTree {
  type ImportMapping = Seq[(String, Option[String])]
}
