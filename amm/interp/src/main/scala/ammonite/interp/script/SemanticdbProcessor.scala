package ammonite.interp.script

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import scala.collection.mutable
import scala.meta.internal.semanticdb._

object SemanticdbProcessor {

  def postProcess(
    module: Script,
    wd: Option[os.Path],
    adjust: Int => (Int, Int) => Option[(Int, Int)],
    target: os.Path,
    originalSource: os.RelPath,
    destSource: os.RelPath
  ): Unit = {
    val semanticDbDir = target / "META-INF" / "semanticdb"
    val orig = semanticDbDir /
      originalSource.segments.init /
      s"${originalSource.last}.semanticdb"
    val dest = semanticDbDir /
      destSource.segments.init /
      s"${destSource.last}.semanticdb"

    val mapRange = {
      val adjust0 = adjust(module.blocks.length)
      range: scala.meta.internal.semanticdb.Range =>
        for {
          (startLine, startChar) <- adjust0(range.startLine, range.startCharacter)
          (endLine, endChar) <- adjust0(range.endLine, range.endCharacter)
        }
          yield range
            .withStartLine(startLine)
            .withStartCharacter(startChar)
            .withEndLine(endLine)
            .withEndCharacter(endChar)
    }

    def updateTrees(trees: Seq[Tree]): Option[Seq[Tree]] =
      trees
        .foldLeft(Option(new mutable.ListBuffer[Tree])) {
          (accOpt, t) =>
            for (acc <- accOpt; t0 <- updateTree(t)) yield acc += t0
        }
        .map(_.result())

    def updateTree(tree: Tree): Option[Tree] =
      tree match {
        case a: ApplyTree =>
          for {
            function <- updateTree(a.function)
            args <- updateTrees(a.arguments)
          } yield a.withFunction(function).withArguments(args)
        case Tree.Empty => Some(Tree.Empty)
        case f: FunctionTree =>
          for {
            body <- updateTree(f.body)
          } yield f.withBody(body)
        case i: IdTree => Some(i)
        case l: LiteralTree => Some(l)
        case m: MacroExpansionTree =>
          for {
            beforeExp <- updateTree(m.beforeExpansion)
          } yield m.withBeforeExpansion(beforeExp)
        case o: OriginalTree =>
          if (o.range.isEmpty) Some(o)
          else
            for {
              range <- o.range.flatMap(mapRange)
            } yield o.withRange(range)
        case s: SelectTree =>
          for {
            qual <- updateTree(s.qualifier)
          } yield s.withQualifier(qual)
        case t: TypeApplyTree =>
          for {
            fun <- updateTree(t.function)
          } yield t.withFunction(fun)
      }

    if (os.isFile(orig)) {
      val docs = TextDocuments.parseFrom(os.read.bytes(orig))
      val updatedDocs = docs.withDocuments {
        val uriOpt =
          for (wd0 <- wd; p <- module.codeSource.path)
            yield wd0.toNIO.toUri.relativize(p.toNIO.toUri).toASCIIString
        docs.documents.map { doc =>
          doc
            .withText(module.code)
            .withUri(uriOpt.getOrElse(doc.uri))
            .withMd5(md5(module.code))
            .withDiagnostics {
              doc.diagnostics.flatMap { diag =>
                diag.range.fold(Option(diag)) { range =>
                  mapRange(range).map(diag.withRange)
                }
              }
            }
            .withOccurrences {
              doc.occurrences.flatMap { occurrence =>
                occurrence.range.fold(Option(occurrence)) { range =>
                  mapRange(range).map(occurrence.withRange)
                }
              }
            }
            .withSynthetics {
              doc.synthetics.flatMap { syn =>
                val synOpt = syn.range.fold(Option(syn)) { range =>
                  mapRange(range).map(syn.withRange)
                }
                synOpt.flatMap { syn0 =>
                  updateTree(syn0.tree)
                    .map(syn0.withTree)
                }
              }
            }
        }
      }
      os.write(dest, updatedDocs.toByteArray, createFolders = true)
    } else
      System.err.println(s"Error: $orig not found (for $dest)")
  }

  private def md5(content: String): String = {
    val md = MessageDigest.getInstance("MD5")
    val digest = md.digest(content.getBytes(StandardCharsets.UTF_8))
    val res = new BigInteger(1, digest).toString(16)
    if (res.length < 32)
      ("0" * (32 - res.length)) + res
    else
      res
  }

}
