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
import oscar.algo.reversible._
import oscar.algo.reversible.ReversibleSparseSet
import oscar.cp.core.variables.{CPBoolVar, CPIntVar, CPVar}

/**
 * or x_i = true
 * @author Pierre Schaus pschaus@gmail.com
 */
class Or(val x: Array[CPBoolVar]) extends Constraint(x(0).store, "Or") {

  var i = 0
  var down = 0
  var up = 0
  val n = x.size

  override def associatedVars(): Iterable[CPVar] = x

  override def setup(l: CPPropagStrength): Unit = {
    i = 0
    while (i < n && x(i).isBound) {
      if (x(i).isBoundTo(1)) return
      i += 1
    }
    down = i
    i = n-1
    while (i >= 0 && x(i).isBound && i >= down) {
      if (x(i).isBoundTo(1)) return
      i -= 1
    }
    up = i
    if (down > up) {
      throw Inconsistency
    } else if (down == up) {
       x(up).assign(1)
    } else {
      assert(down != up)
      x(down).callValBindIdxWhenBind(this,down)
      x(up).callValBindIdxWhenBind(this,up)
      
      //x(up).callPropagateWhenBind(this,false)
      //x(down).callPropagateWhenBind(this,false)
    }
  }

  override def valBindIdx(y: CPIntVar, idx: Int): Unit = {
    if (y.isBoundTo(1)) {
      deactivate()
      return
    }

    if (down >= n || up < 0 || x(down).isBound || x(up).isBound || (down >= up)) {
      i = 0
      while (i < n && x(i).isBound) {
        if (x(i).isBoundTo(1)) {
          deactivate()
          return
        }
        i += 1
      }
      down = i
      i = n - 1
      while (i >= 0 && x(i).isBound && i >= down) {
        if (x(i).isBoundTo(1)) {
          deactivate()
          return
        }
        i -= 1
      }
      up = i
    } 
    if (down > up) {
      throw Inconsistency
    }
    else if (down == up) { // only one unassigned var
      x(up).assign(1)
      deactivate()
    }
    else {
      assert(down != up)
      assert(x(down).isBound == false)
      assert(x(up).isBound == false)
      x(down).callValBindIdxWhenBind(this,down)
      x(up).callValBindIdxWhenBind(this,up)
    }
  }
  
  override def propagate(): Unit = {
    if (down < n && x(down).isBoundTo(1)) {
      deactivate()
      return
    }
    if (up >= 0 && x(up).isBoundTo(1)) {
      deactivate()
      return
    }
    if (down >= n || up < 0 || x(down).isBound || x(up).isBound || (down >= up)) {
      i = 0
      while (i < n && x(i).isBound) {
        if (x(i).isBoundTo(1)){
          deactivate()
          return
        }
        i += 1
      }
      down = i
      i = n - 1
      while (i >= 0 && x(i).isBound && i >= down) {
        if (x(i).isBoundTo(1)){
          deactivate()
          return
        }
        i -= 1
      }
      up = i
    } 
    if (down > up) {
      throw Inconsistency
    }
    else if (down == up) { // only one unassigned var
      x(up).assign(1)
      deactivate()
      return
    }
    else {
      assert(down != up)
      assert(x(down).isBound == false)
      assert(x(up).isBound == false)
      x(down).callPropagateWhenBind(this)
      x(up).callPropagateWhenBind(this)
    }
  }  
}

