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
import oscar.algo.reversible.ReversibleSparseSet
import oscar.cp.core.variables.{CPBoolVar, CPIntVar, CPVar}

/**
 * and_i x_i <--> bi 
 * @author Pierre Schaus pschaus@gmail.com
 */
class And(val X: Array[CPBoolVar], val b: CPBoolVar) extends Constraint(b.store, "AndReif") {

  override def associatedVars(): Iterable[CPVar] = X ++ Array(b)

  var unbound: ReversibleSparseSet = null
  priorityBindL1 = CPStore.MaxPriorityL1-1
  
  
  override def setup(l: CPPropagStrength): Unit = {
    if (X.size == 2) {
      s.post(new BinaryAnd(X(0),X(1),b))
      this.deactivate()
      return
    }
    unbound = new ReversibleSparseSet(s,0,X.size-1)
    X.foreach(_.callPropagateWhenBind(this))
    for ((x,i) <- X.zipWithIndex) {
      if (x.isBound) {
        valBindIdx(x,i)
        if(!this.isActive)
          return
      }
      else
        x.callValBindIdxWhenBind(this,i)
    }
    b.callPropagateWhenBind(this)
    propagate()
  }
  
  override def valBindIdx(x: CPIntVar, idx: Int): Unit = {
    if (x.isBoundTo(0)) {
      b.assign(0)
      this.deactivate()
    } else {
    	unbound.removeValue(idx)
    	if (unbound.isEmpty) {
    	  b.assign(1)
        this.deactivate()
    	}
    }
  }
  
  override def propagate(): Unit = {
    if (b.isBoundTo(1)) {
      for (i <- unbound)
        X(i).assign(1)
      this.deactivate()
    }
    else if (b.isBoundTo(0) && unbound.size == 1) {
      // at least one must be = 0
      X(unbound.min).assign(0)
      this.deactivate()
    }
  }
}


