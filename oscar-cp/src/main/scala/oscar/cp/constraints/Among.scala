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
import oscar.algo.reversible.ReversibleSparseSet
import oscar.cp.core.variables.{CPIntVar, CPVar}

/**
 * Implementation of Among Constraint:
 *   N variables of X take a values in set
 * @author Pierre Schaus pschaus@gmail.com
 */
class Among(val N: CPIntVar, val X: Array[CPIntVar], val S: Set[Int]) extends Constraint(N.store, "Among") {

  override def associatedVars(): Iterable[CPVar] = Array(N) ++ X

  override def setup(l: CPPropagStrength): Unit = {
    // for each xi, maintain the size of the intersection between D(xi) and v
    val interSize = Array.tabulate(X.size)(i => new ReversibleInt(s, X(i).count(v => S.contains(v))))
    val nonEmptyInterIdx = new ReversibleSparseSet(s, 0, X.size - 1)
    val nonSubsetIdx = new ReversibleSparseSet(s, 0, X.size - 1)
    for (i <- 0 until X.size) {
      if (interSize(i).value == 0)
        nonEmptyInterIdx.removeValue(i)
      if (interSize(i).value == X(i).size)
        nonSubsetIdx.removeValue(i)
    }
    val lb = new ReversibleInt(s, (0 until X.size).count(i => interSize(i).value == X(i).size))
    val ub = new ReversibleInt(s, (0 until X.size).count(i => interSize(i).value > 0))
    
    N.updateMin(lb.value)
    N.updateMax(ub.value)

    def filterMaxCount(): Unit = {
      // if lb = max(N), it means no more value can be from the set. 
      // every variables not yet surely in the set must be removed the values from the set
      for (i <- nonSubsetIdx) {
    	  for (v <- S) {
    	    X(i).removeValue(v)
    	  }
      }
    }
    
    def filterMinCount(): Unit = {
      // if ub = min(N), it mean all the values still with non empty intersection must be in the set. 
      // every variable with non empty intersection must only keep values from the set
      for (i <- nonEmptyInterIdx) {
    	  var Dxi = X(i).iterator
    	  for (v <- Dxi; if !S.contains(v)) {
    	    X(i).removeValue(v)
    	  }
      }
    }

    def filter(): Boolean = {
      N.updateMin(lb.value)
      N.updateMax(ub.value)
      if (lb.value == N.max) {
        filterMaxCount()
        true
      }
      else if (ub.value == N.min) {
        filterMinCount()
        true
      }
      else
        false
    }

    // for every variables not yet included and not disjoint from S
    for (i <- 0 until X.size; if interSize(i).value < X(i).size && interSize(i).value > 0) {
      X(i).filterWhenDomainChangesWithDelta() { d =>
        if (X(i).isBound) {
          val v = X(i).min
          if (S.contains(v)) {
            interSize(i).value = 1
          } else {
            interSize(i).value = 0
          }
        } else {
          // X(i) lost some values values
          for (v <- d.values; if (S.contains(v))) {
            interSize(i).decr
          }
        }

        if (interSize(i).value == X(i).size) {
          lb.incr()
          nonSubsetIdx.removeValue(i)
        }
        if (interSize(i).value == 0) {
          ub.decr()
          nonEmptyInterIdx.removeValue(i)
        }

        //we are done if...
        filter() || interSize(i).value == 0 || interSize(i).value == X(i).size
      }
    }
    N.filterWhenBoundsChange() {
      filter()
    }
    if(filter())
      deactivate()
  }

}


