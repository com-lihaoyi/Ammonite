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

import Compat._
import xsbti.InteractiveConsoleResult

object InteractiveConsoleHelper {
  implicit def toConsoleResult(ir: Results.Result): InteractiveConsoleResult =
    ir match {
      case Results.Success    => InteractiveConsoleResult.Success
      case Results.Incomplete => InteractiveConsoleResult.Incomplete
      case Results.Error      => InteractiveConsoleResult.Error
    }
}
