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

import oscar.cp.core._
import oscar.algo.reversible._
import oscar.cp.core.variables.{CPSetVar, CPVar}
import oscar.cp.core.delta.{DeltaSetVar, PropagatorSetVar}

/**
 * Implementation of Disjoint Constraint (two sets must be disjoint)
 * @author Pierre Schaus pschaus@gmail.com
 */
class Disjoint(val x: CPSetVar, val y: CPSetVar) extends Constraint(x.store, "Disjoint") {

  override def associatedVars(): Iterable[CPVar] = Array(x, y)

  override def setup(l: CPPropagStrength): Unit = {
    
    def filterY(d: DeltaSetVar): Unit = {
      for (v <- d.deltaRequired) {
        y.excludes(v)
      }
    }
    
    def filterX(d: DeltaSetVar): Unit = {
      for (v <- d.deltaRequired) {
        x.excludes(v)
      }
    }
    
    x.filterWhenDomainChangesWithDelta() { d =>
    	filterY(d)
    }
    
    y.filterWhenDomainChangesWithDelta() { d =>
    	filterX(d)
    }
    
    for (v <- x.requiredValues) {
      y.excludes(v)
    }

    for (v <- y.requiredValues) {
      x.excludes(v)
    }
  }

}


