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

package oscar.cp.constraints

import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPIntVar, CPIntVarViewMinus, CPIntVarViewOffset, CPVar}

/** author Renaud Hartert ren.hartert@gmail.com */
final class SoftAllDifferent(variables: Array[CPIntVar], violations: CPIntVar) extends Constraint(violations.store, "SoftAllDifferent") {

  override def associatedVars(): Iterable[CPVar] = variables ++ Array(violations)

  final override def setup(l: CPPropagStrength): Unit = {
    val nVariables = variables.length
    val nValues = new CPIntVarViewOffset(new CPIntVarViewMinus(violations), nVariables)
    if (l == CPPropagStrength.Weak) s.post(new AtLeastNValueFWC(variables, nValues))
    else if (l == CPPropagStrength.Medium) s.post(new AtLeastNValueFWC(variables, nValues))
    else if (l == CPPropagStrength.Strong) s.post(new AtLeastNValueAC(variables, nValues))
    else sys.error("Unknown propagation level.")
  } 
}