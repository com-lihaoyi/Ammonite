/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Scala Center, Lightbend, and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * SPDX-License-Identifier: Apache-2.0
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package xsbt

object trace extends TraceSyntax {
  object disable extends TraceSyntax {
    override def apply[T](q: String)(op: => T): T = op
  }
  object enable extends TraceSyntax
}

object TraceSyntax {
  private var indent = 0
}
trait TraceSyntax {
  import TraceSyntax._
  def apply[T](q: String)(op: => T): T = {
    val margin = "  " * indent
    println(s"$margin$q?")
    val res =
      try {
        indent += 1
        op
      } finally indent -= 1
    println(s"$margin$q = $res")
    res
  }
}
