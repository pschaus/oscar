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

import oscar.cp.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPIntVar, CPVar}

/**
 * Channel 0/1 constraint
 *
 * \forall i : x(i) == 1   <==> pos == i
 * \exists i : x(i) == 1
 */
class Channel(x: Array[CPIntVar], pos: CPIntVar) extends Constraint(pos.store) {
  override def associatedVars(): Iterable[CPVar] = x ++ Array(pos)

  override def setup(l: CPPropagStrength): Unit = {
    pos.updateMin(0)
    pos.updateMax(x.length-1)

    pos.callValRemoveWhenValueIsRemoved(this)
    pos.callValBindIdxWhenBind(this, -1)

    x.zipWithIndex.foreach{case (v, idx) => v.callValBindIdxWhenBind(this, idx)}

    var i = 0
    while(i != x.length) {
      x(i).callValBindIdxWhenBind(this, i)
      if(x(i).isBound)
        valBindIdx(x(i), i)
      i += 1
    }

    i = 0
    while(i != x.length) {
      if(!pos.hasValue(i)) {
        x(i).assign(0)
      }
      i += 1
    }

    if(pos.isBound) {
      x(pos.min).assign(1)
      this.deactivate()
    }
  }

  override def valRemove(pos: CPIntVar, value: Int): Unit = {
    x(value).assign(0)
  }

  override def valBindIdx(xvar: CPIntVar, idx: Int): Unit = {
    if(idx == -1) {
      var i = 0
      while (i != x.length) {
        x(i).assign(if (i == pos.min) 1 else 0)
        i += 1
      }
      this.deactivate()
    }
    else {
      if(xvar.isBoundTo(0))
        pos.removeValue(idx)
      else
        pos.assign(idx)
    }
  }
}
