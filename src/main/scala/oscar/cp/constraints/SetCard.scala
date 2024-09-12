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
import oscar.cp.core._
import oscar.cp.core.variables.{CPIntVar, CPSetVar, CPVar}

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class SetCard(val x: CPSetVar, val c: CPIntVar) extends Constraint(x.store, "SetCard") {
  priorityL2 = CPStore.MaxPriorityL2

  override def associatedVars(): Iterable[CPVar] = Array(x, c)

  override def setup(l: CPPropagStrength): Unit = {
    x.callPropagateWhenDomainChanges(this)
    c.callPropagateWhenBoundsChange(this)
    propagate()
  }
  
  override def propagate(): Unit = {
    c.updateMin(x.requiredSize)
    c.updateMax(x.possibleSize)
    
    if (c.min > x.possibleSize) throw Inconsistency
    else if (c.max < x.requiredSize) throw Inconsistency
    else if (c.min == x.possibleSize) {
      // every possible must become required
      x.requiresAll()
      c.updateMax(c.min)
    } else if (c.max == x.requiredSize) {
      // removes every possible not required
      x.excludesAll()
      c.updateMin(c.max)
    } else {
      c.updateMin(x.requiredSize)
      c.updateMax(x.possibleSize)
    }
  }
}
