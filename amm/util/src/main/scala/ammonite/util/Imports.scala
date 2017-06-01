/**
  * Various common, "dumb" data-structures that represent common things that
  * are passed around inside Ammonite
  */
package ammonite.util

import ammonite.util.Util.{ClassFiles, VersionedWrapperId, newLine}

import scala.collection.mutable

/**
  * The serialized output of running a script, including both metadata and the classfile binaries
  */
case class ScriptOutput(processed: ScriptOutput.Metadata, classFiles: Seq[ClassFiles])
object ScriptOutput{
  /**
    * Metadata extracted from the compilation of a single block, without the classfiles
    * but with enough information to fetch the classfiles form disk and evaluate the
    * block without compiling/parsing it
    */
  case class BlockMetadata(id: VersionedWrapperId,
                           leadingSpaces: String,
                           hookInfo: ImportHookInfo,
                           finalImports: Imports)
  case class Metadata(blockInfo: Seq[BlockMetadata])
}

case class ImportHookInfo(imports: Imports,
                          stmts: Seq[String],
                          trees: Seq[ImportTree])

case class Evaluated(wrapper: Seq[Name],
                     imports: Imports)

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
case class ImportData(fromName: Name,
                      toName: Name,
                      prefix: Seq[Name],
                      importType: ImportData.ImportType)


object ImportData{
  sealed case class ImportType(name: String)
  val Type = ImportType("Type")
  val Term = ImportType("Term")
  val TermType = ImportType("TermType")
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
class Imports private (val value: Seq[ImportData]){
  def ++(others: Imports) = Imports(this.value, others.value)
  override def toString() = {
    // Group the remaining imports into sliding groups according to their
    // prefix, while still maintaining their ordering
    val grouped = mutable.Buffer[mutable.Buffer[ImportData]]()
    for(data <- value){
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
    val out = for(group <- grouped) yield {
      val printedGroup = for(item <- group) yield{
        if (item.fromName == item.toName) item.fromName.backticked
        else s"${item.fromName.backticked} => ${item.toName.backticked}"
      }
      val pkgString = Util.encodeScalaSourcePath(group.head.prefix)
      "import " + pkgString + s".{$newLine  " +
        printedGroup.mkString(s",$newLine  ") + s"$newLine}$newLine"
    }
    out.mkString
  }
}

object Imports{
  // This isn't called directly, but we need to define it so uPickle can know
  // how to read/write imports
  def unapply(s: Imports): Option[Seq[ImportData]] = Some(s.value)
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
    for(data <- importData.reverseIterator){
      val stomped = data.importType match{
        case ImportData.Term => Seq(stompedTerms)
        case ImportData.Type => Seq(stompedTypes)
        case ImportData.TermType => Seq(stompedTerms, stompedTypes)
      }
      if (!stomped.exists(_(data.toName))){
        out.append(data)
        stomped.foreach(_.add(data.toName))
        data.prefix.headOption.foreach(stompedTerms.remove)
      }
    }
    new Imports(out.reverse)
  }
}
