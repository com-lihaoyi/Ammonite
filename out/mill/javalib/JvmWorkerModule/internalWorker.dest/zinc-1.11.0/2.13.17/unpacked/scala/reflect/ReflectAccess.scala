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

package scala.reflect

import scala.tools.nsc.Global

// Gives access to `private[reflect] def compactifyName`
object ReflectAccess {
  def flattenedOwner(g: Global)(sym: g.Symbol): g.Symbol = {
    val chain = sym.owner.ownerChain.dropWhile(o => o.isClass && !o.hasPackageFlag)
    if (chain.isEmpty) g.NoSymbol else chain.head
  }

  def flattenedName(g: Global)(sym: g.Symbol): g.Name = {
    val owner = sym.owner
    val prefix =
      if (owner.isRoot || owner == g.NoSymbol || owner.hasPackageFlag) ""
      else "" + flattenedName(g)(owner) + NameTransformer.NAME_JOIN_STRING
    val flat = prefix + sym.rawname
    val nameString = if (owner.isJava) flat else g.compactifyName(flat)
    if (sym.isType) g.newTypeNameCached(nameString) else g.newTermNameCached(nameString)
  }
}
