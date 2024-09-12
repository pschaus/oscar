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

package oscar.cp.core.watcher

import oscar.cp.core.variables.CPSetVar
import oscar.cp.core.Constraint

/**
 * Trailable Queue of AC5 events
 * Each entry of the queue stores:
 *  - a index
 *  - a variable
 *  @author Pierre Schaus pschaus@gmail.com
 */
class PropagEventQueueVarSet(val next: PropagEventQueueVarSet, val cons: Constraint, val x: CPSetVar, val idx: Int) {
	
    def this(next: PropagEventQueueVarSet, cons: Constraint, x: CPSetVar) = {
      this(next,cons,x,0)
    }
    
    def hasNext() = next != null

	override def toString(): String = "PropagEventQueueVarSet constraint:"+cons+" var:"+x+" idx:"+idx;
	
	
	def size() = {
		var s = 0;
		var q = this;
		while (q != null) {
			s += 1
			q = q.next
		}
		s
	}

}
