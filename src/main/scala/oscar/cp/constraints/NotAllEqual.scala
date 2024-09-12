/*******************************************************************************
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
 ******************************************************************************/
package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.algo.reversible.{ReversibleBoolean, ReversibleInt}
import oscar.cp.core._
import oscar.cp.core.variables.{CPIntVar, CPVar}

/**
 * Ensure that at least two values in x are different (they are not all equal)
 * @author Guillaume Derval <guillaume.derval@uclouvain.be>
 */
class NotAllEqual(val x: Array[CPIntVar]) extends Constraint(x(0).store, "NotAllEqual") {
  val firstValueFound = new ReversibleBoolean(s,false)
  var firstValue: Int = 0
  val nUnbound = new ReversibleInt(s,x.length)

  override def associatedVars(): Iterable[CPVar] = x

  override def setup(l: CPPropagStrength): Unit = {
    // Check specific cases
    if(x.length == 1)
      return
    if(x.length == 2) {
      x(0).store.post(x(0).diff(x(1)))
      return
    }

    x.zipWithIndex.foreach{case (v, idx) => {
      v.callValBindIdxWhenBind(this, idx)

      // If the variable is already bound, call valBindIdx
      if(v.isBound)
        valBindIdx(v, idx)

      if(!isActive)
        return
    }}

    if(nUnbound.getValue() == 0)
      throw Inconsistency//if we have all our variable bound but did not return Success earlier, we failed
  }

  override def valBindIdx(x: CPIntVar, idx: Int): Unit = {
    nUnbound.decr()
    if(firstValueFound.getValue()) { //if we already found our first value
      if(x.min != firstValue) {
        deactivate()
        return
      }
    }
    else {
      firstValueFound.setTrue()
      firstValue = x.min
    }
    if(nUnbound.getValue() == 0)
      throw Inconsistency
  }
}


