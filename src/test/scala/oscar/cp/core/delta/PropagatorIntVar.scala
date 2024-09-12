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

package oscar.cp.core.delta

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.CPStore

/**
 * Create a new constrait, with the closure `filter` as propagator.
 * @param x
 * @param id
 * @param filter The propagator to be used; Should return true only if the propagator is done (== it should be deactivated)
 * @param name
 */
class PropagatorIntVar(x: CPIntVar, id: Int, filter: DeltaIntVar => Boolean, name: String = "PropagatorIntVar") extends Constraint(x.store, name) {

  override def associatedVars(): Iterable[CPVar] = Array(x)

  private[this] val _delta = x.delta(this,id)
  
  final def priority: Int = this.priorityL2
  final def priority_=(level: Int): Unit = this.priorityL2 = level
  
  @inline final def snapshot = _delta
  
  override def setup(l: CPPropagStrength): Unit = {}
  
  override def propagate(): Unit = if(filter(_delta)) deactivate()
}