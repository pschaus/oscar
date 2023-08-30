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
import oscar.cp.core.variables.{CPIntVar, CPVar}

/**
 * Soft Global Cardinality Constraint
 * @author Victor Lecomte
 */
class SoftGCC(X: Array[CPIntVar], minVal: Int, lower: Array[Int], upper: Array[Int], viol: CPIntVar)
  extends Constraint(X(0).store, "SoftGCC") {

  override def associatedVars(): Iterable[CPVar] = X ++ Array(viol)

  override def setup(l: CPPropagStrength): Unit = {
    val ok = l match {
      case CPPropagStrength.Strong => s.post(new SoftGCCAC(X, minVal, lower, upper, viol))
      case _ => s.post(new SoftGCCFWC(X, minVal, lower, upper, viol))
    }
  }
}

