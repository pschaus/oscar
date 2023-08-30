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
 *******************************************************************************/


package oscar.cp.scheduling.constraints

import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.{CPPropagStrength, Constraint}

/**
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 */
class BinaryDisjunctiveWithTransitionTimes(startVar1: CPIntVar, endVar1: CPIntVar, startVar2: CPIntVar, endVar2: CPIntVar, transition1To2: Int, transition2To1: Int) extends Constraint(startVar1.store) {

  idempotent = true
  
  override def setup(l: CPPropagStrength): Unit = {
    startVar1.callPropagateWhenBoundsChange(this)
    endVar1.callPropagateWhenBoundsChange(this)
    startVar2.callPropagateWhenBoundsChange(this)
    endVar2.callPropagateWhenBoundsChange(this)
    propagate()
  }

  override def propagate(): Unit = {
    if (endVar1.min + transition1To2 > startVar2.max) {
      startVar1.updateMin(endVar2.min + transition2To1)
      endVar2.updateMax(startVar1.max - transition2To1)
    }
    else if (endVar2.min + transition2To1 > startVar1.max) {
      startVar2.updateMin(endVar1.min + transition1To2)
      endVar1.updateMax(startVar2.max - transition1To2)
    }
  }

  override def associatedVars(): Iterable[CPVar] = Array(startVar1, endVar1, startVar2, endVar2)
}