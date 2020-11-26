package ammonite.compiler

import ammonite.compiler.iface.{CodeSource, Imports}
import ammonite.util._
import ammonite.util.InterfaceExtensions._
import ammonite.util.Util.{newLine, normalizeNewlines}

object CodeClassWrapper extends ScalaCodeWrapper {
  /*
   * The goal of this code wrapper is that the user code:
   * - should be in a class rather than a singleton,
   * - should see the previous commands results via instances of these classes,
   *   not referencing singletons along the way.
   *
   * Only dealing with class instances at runtime, rather than singletons, behaves
   * well wrt Java serialization. Singletons don't write their fields during serialization,
   * and re-compute them when deserialized. On the other hand, class instances serialize
   * and de-serialize their fields, as expected.
   *
   * It still allows users to wrap code in singletons rather than a class if they want to:
   * user code that solely consists of a singleton, is itself wrapped in a singleton,
   * rather than a class. This is useful for macro code, or definitions that are
   * themselves processed by macros (definitions in objects are easier to process from
   * macros).
   */
  private val userCodeNestingLevel = 2
  private val q = "\""
  private val tq = "\"\"\""
  override val wrapperPath: Seq[Name] = Seq(Name("instance"))
  def apply(
    code: String,
    source: CodeSource,
    imports: Imports,
    printCode: String,
    indexedWrapperName: Name,
    extraCode: String
  ) = {
    val pkgName = source.pkgName
    val isObjDef = Parsers.isObjDef(code)

    if (isObjDef) {
      val top = normalizeNewlines(s"""
package ${pkgName.head.encoded}
package ${Util.encodeScalaSourcePath(pkgName.tail)}

${imports.repr}

object ${indexedWrapperName.backticked}{
  val instance: Helper.type = Helper
  def $$main() = instance.$$main()

  object Helper extends java.io.Serializable {
"""
      )

      val bottom = normalizeNewlines(s"""\ndef $$main() = { $printCode }
  override def toString = "${indexedWrapperName.encoded}"
  $extraCode
}}
""")

      (top, bottom, userCodeNestingLevel)
    } else {

      val (reworkedImports, reqVals) = {

        val (l, reqVals0) = imports
          .data
          .toSeq
          .map { data =>
            val prefix = Seq(Name("_root_"), Name("ammonite"), Name("$sess"))
            if (data.prefix.startsWith(prefix) && data.prefix.endsWith(wrapperPath)) {
              val name = data.prefix.drop(prefix.length).dropRight(wrapperPath.length).last
              (data.withPrefix(Seq(name)), Seq(name -> data.prefix))
            } else
              (data, Nil)
          }
          .unzip

        (new Imports(l.toArray), reqVals0.flatten)
      }

      val requiredVals = reqVals
        .distinct
        .groupBy(_._1)
        .mapValues(_.map(_._2))
        .toVector
        .sortBy(_._1.raw)
        .collect {
          case (key, Seq(path)) =>
            /*
             * Via __amm_usedThings, that itself relies on the *-tree.txt resources generated
             * via the AmmonitePlugin, we can know whether the current command uses things from
             * each of the previous ones, and null-ify the references to those that are unused.
             * That way, the unused commands don't prevent serializing this command.
             */
            val encoded = Util.encodeScalaSourcePath(path)
            s"final val ${key.backticked}: $encoded.type = " +
              s"if (__amm_usedThings($tq${key.raw}$tq)) $encoded else null$newLine"
          case (key, values) =>
            throw new Exception(
              "Should not happen - several required values with the same name " +
                s"(name: $key, values: $values)"
            )
        }
        .mkString

      val usedThingsSet =
        if (reqVals.isEmpty) ""
        else
          s"""
  @_root_.scala.transient private val __amm_usedThings =
    _root_.scala.Predef.genericArrayOps(""" +
    "_root_.ammonite.repl.ReplBridge.value.usedEarlierDefinitions).iterator.toSet"

        val top = normalizeNewlines(s"""
package ${pkgName.head.encoded}
package ${Util.encodeScalaSourcePath(pkgName.tail)}

object ${indexedWrapperName.backticked}{
  val wrapper = new ${indexedWrapperName.backticked}
  val instance = new wrapper.Helper
  def $$main() = instance.$$main()
}

final class ${indexedWrapperName.backticked} extends java.io.Serializable {

$usedThingsSet

  override def toString = $q${indexedWrapperName.encoded}$q
$requiredVals
${reworkedImports.repr}

final class Helper extends java.io.Serializable{\n"""
    )

      val bottom = normalizeNewlines(s"""\ndef $$main() = { $printCode }
  override def toString = "${indexedWrapperName.encoded}"
  $extraCode
}}
""")

      (top, bottom, userCodeNestingLevel)
    }
  }
}
