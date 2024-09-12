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
 * *****************************************************************************/
package oscar.cp.scheduling.constraints

import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class MaxCumulativeCapaCheck(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int) extends Constraint(capacity.store, "MaxSweepCumulative") {

  private val nTasks = starts.size
  private val Tasks = 0 until nTasks

  override def associatedVars(): Iterable[CPVar] = starts ++ durations ++ ends ++ demands ++ resources ++ Array(capacity)

  override def setup(l: CPPropagStrength): Unit = {
    capacity.callPropagateWhenBoundsChange(this)    
    return propagate()
  }
  
  override def propagate(): Unit = {
    for (i <- Tasks; if durations(i).min > 0) {
      if (demands(i).min > capacity.max) {
        resources(i).removeValue(id)
      } 
    }
    if (Tasks.forall(i => durations(i).max == 0 || demands(i).max < capacity.min || !resources(i).hasValue(id)))
      this.deactivate()
  }
}
  

