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
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.delta.DeltaIntVar


/**
 * Implementation of Count Constraint:
 *   N variables of X take the values Y
 * @author Pierre Schaus pschaus@gmail.com
 */
class Count(val N: CPIntVar, val X: Array[CPIntVar], val Y: CPIntVar) extends Constraint(N.store, "Among") {

    /*
     * propagate X & Y -> N
     * ------------------
     * countvmin = nb variables in X bound to v
     * countvmax = nb variables in X having v in its dom
     * Nmin = min_{v in D(Y)} countvmin
     * Nmax = max_{v in D(Y)} countvmax
     * 
     * incremental:
     * supportmin = v in D(Y) such that countvmin = Nmin
     * supportmax = v in D(Y) such that countvmax = Nmax
     * 
     * recompute supportmin/max if not in D(Y) or if its value changes
     * in this case, refilter Nmin/Nmax 
     * 
     * 
     * propagate X & N -> Y
     * -------------------
     * a value v does not appear at least Nmin times in D(Xi)'s => remove it from D(Y)
     * a value v is bound to more Nmax times in D(Xi)'s => remove it from D(Y)
     * 
     * propagate Y & N -> X
     * ----------------------
     * if Y is bound to v:
     * when countvmin = Nmax = remove the value v from unbound variables
     * when countvmax = Nmin = assign the value v to all variables having it
     * 
     * Filtering given by the decomposition with reified constraints:
     * When at most Nmin variables have a non empty intersection with Y, those variables must be equal to Y
     */

  override def associatedVars(): Iterable[CPVar] = X ++ Array(N, Y)

  override def setup(l: CPPropagStrength): Unit = {
    
    val minY = Y.min
    val maxY = Y.max
    
    val cmin = Array.tabulate(maxY-minY+1)(v => new ReversibleInt(s,X.count(_.isBoundTo(v+minY))))
    val cmax = Array.tabulate(maxY-minY+1)(v => new ReversibleInt(s,X.count(_.hasValue(v+minY))))
    val supportmin = new ReversibleInt(s,0)
    val supportmax = new ReversibleInt(s,0)
    def countmin(v: Int) = cmin(v-minY)
    def countmax(v: Int) = cmax(v-minY)
    
    var updateSupportMinRequired = true
    def updateSupportMin(): Int = {
      var currMin = Int.MaxValue
      for (v <- Y) {
        if (countmin(v).value < currMin) {
          currMin = countmin(v).value
          supportmin.value = v
        }
      }
      updateSupportMinRequired = false
      currMin
    }
    
    
    var updateSupportMaxRequired = true
    def updateSupportMax(): Int = {
      var currMax = Int.MinValue
      for (v <- Y) {
        if (countmax(v).value > currMax) {
          currMax = countmax(v).value
          supportmax.value = v
        }
      }
      updateSupportMaxRequired = false
      currMax
    }
    
    def filterYBound(): Boolean = {
      assert(Y.isBound)
      val v = Y.min
      val mincount = X.count(_.isBoundTo(v))
      val maxcount = X.count(_.hasValue(v))
      N.updateMin(mincount)
      N.updateMax(maxcount)
      
      if (mincount == N.max) {
        // remove the value v from unbound variables
        for (x <- X; if !x.isBound) {
          x.removeValue(v)
        }
      }
      if (maxcount == N.min) {
        // assign the value v to all variables having it
        for (x <- X; if !x.isBound && x.hasValue(v)) {
          x.assign(v)
        }
      }
      false
    }

    def updateN(): Unit = {
      if (updateSupportMinRequired)
        N.updateMin(updateSupportMin())
      if (updateSupportMaxRequired)
        N.updateMax(updateSupportMax())
    }
        
    def updateLostValue(v: Int): Unit = {
      if (Y.hasValue(v)) {
        if (supportmax.value == v) {
          updateSupportMaxRequired = true
        }
        countmax(v).decr
        if (countmax(v).value < N.min) {
          Y.removeValue(v)
        }
      }
    }
    
    def updateBindValue(v: Int): Unit = {
      if (Y.hasValue(v)) {
        if (supportmin.value == v) {
          updateSupportMinRequired = true
        }
        countmin(v).incr()
        if (countmin(v).value > N.max) {
          Y.removeValue(v)
        }
      }
    }
    
    Y.filterWhenDomainChangesWithDelta() { d: DeltaIntVar =>
      // should test in constant time
      if (!Y.hasValue(supportmax.value)) {
        updateSupportMaxRequired = true
      }
      if (!Y.hasValue(supportmin.value)) {
        updateSupportMinRequired = true
      }
      updateN()
      N.isBound
    }
    
    Y.filterWhenBind() {
    	filterYBound()
    }
    
    def filterX(x: CPIntVar, d: DeltaIntVar): Boolean = {
      //println("FilterX"+X.mkString(",")+" Y:"+Y)
      for (v <- d.values) {
        //println("lost value"+v)
        updateLostValue(v)
      }
      if (x.isBound) {
        //println("is now bound")
        updateBindValue(x.min)
      }
      updateN()
      if (Y.isBound) filterYBound()
      else if(N.isBound) true
      else false
    }
    
    for (x <- X; if !x.isBound) {
      x.filterWhenDomainChangesWithDelta() {d: DeltaIntVar =>
        filterX(x,d)
      }
    }
    
    updateN()

    if (N.isBound) {
      deactivate()
      return
    }

    if (Y.isBound) {
      filterYBound()
      return
    }

    s.post(new Sum(X.map((_ ?=== Y)), N))
    deactivate()
  }

}


