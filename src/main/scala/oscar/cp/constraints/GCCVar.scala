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
import oscar.cp.constraints._
import oscar.cp.modeling._
import oscar.algo.DisjointSets

import scala.collection.mutable.ArrayBuffer
import oscar.cp.core.variables.{CPIntVar, CPVar}

/**
 * Global Cardinality Constraint
 * Constraint the values minval+i cards(i) times in x
 * @author Pierre Schaus pschaus@gmail.com
 */
class GCCVar(x: Array[CPIntVar], val minVal: Int, val cards: Array[CPIntVar]) extends Constraint(x(0).store) {

  override def associatedVars(): Iterable[CPVar] = x ++ cards

  override def setup(l: CPPropagStrength): Unit = {
    l match {
      case CPPropagStrength.Automatic => {
        s.post(new GCCVarFWC(x, minVal, cards))
      }
      case CPPropagStrength.Weak => {
        s.post(new GCCVarFWC(x, minVal, cards))
      }
      case CPPropagStrength.Medium => {
        //s.post(new GCCVarAC(x.toArray, minVal, cards))
        s.post(new GCCVarFWC(x, minVal, cards))
      }
      case CPPropagStrength.Strong => {
        s.post(new GCCVarAC(x.toArray, minVal, cards))
      }
    }
  }
  

}
