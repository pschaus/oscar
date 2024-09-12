/**
 * *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */
package oscar.cp.constraints

import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.CPStore

/**
 * Inverse
 *
 *  This constraint enforces the following rules:
 *  1. prev(next(i)) == i
 *  2. next(prev(i)) == i
 *
 *  @author Renaud Hartert ren.hartert@gmail.com
 */

class Inverse(prev: Array[CPIntVar], next: Array[CPIntVar]) extends Constraint(prev.head.store, "Inverse") {

  override def associatedVars(): Iterable[CPVar] = prev ++ next

  // Checks the consistency of the arguments
  require(prev.length == next.length, "input arrays must have the same size")

  // Structure used to collect removed values
  private[this] val removedValues = new Array[Int](prev.length)

  override def setup(l: CPPropagStrength): Unit = {
    init()
    var i = prev.length
    while (i > 0) {
      i -= 1
      if (!prev(i).isBound) prev(i).callOnChangesIdx(i, s => propagatePrev(s))
      if (!next(i).isBound) next(i).callOnChangesIdx(i, s => propagateNext(s))
    }
  }

  @inline private def propagatePrev(delta: DeltaIntVar): Boolean = {
    val varId = delta.id
    val intVar = prev(varId)
    if (intVar.isBound) next(intVar.min).assign(varId)
    else {
      var i = delta.fillArray(removedValues)
      while (i > 0) {
        i -= 1
        val value = removedValues(i)
        next(value).removeValue(varId)
      }
    }
    false
  }

  @inline private def propagateNext(delta: DeltaIntVar): Boolean = {
    val varId = delta.id
    val intVar = next(varId)
    if (intVar.isBound) prev(intVar.min).assign(varId)
    else {
      var i = delta.fillArray(removedValues)
      while (i > 0) {
        i -= 1
        val value = removedValues(i)
        prev(value).removeValue(varId)
      }
    }
    false
  }
  
  @inline private def init(): Unit = {
    var i = 0
    while (i < prev.length) {
      // Initializes the bounds of the variables
      initBounds(prev(i))
      initBounds(next(i))

      var j = 0
      while (j < prev.length) {
        // Initializes inner domains
        init(prev, next, i, j)
        init(next, prev, i, j)
        j += 1
      }
      i += 1
    }
  }

  @inline private def initBounds(intVar: CPIntVar): Unit = {
    intVar.updateMin(0)
    intVar.updateMax(prev.length - 1)
  }

  @inline private def init(vector1: Array[CPIntVar], vector2: Array[CPIntVar], i: Int, j: Int): Unit = {
    if (!vector1(i).hasValue(j))
      return
    else if (vector1(i).isBound)
      vector2(j).assign(i)
    else if (!vector2(j).hasValue(i))
      vector1(i).removeValue(j)
  }
}

