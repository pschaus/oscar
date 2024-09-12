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
 * Prefix Cardinality Constraint
 * For a description of the arguments see PrefixCCSegments.
 * For a better description of the problem see
 *   https://github.com/vlecomte/prefixcc-tech-report/blob/master/report.pdf
 * For the reasoning behind the choice of constraint according to the strength, see
 *   https://github.com/vlecomte/prefixcc-tech-report/blob/master/perf-comparison.pdf
 * @author Victor Lecomte
 */
class NestedGCC(X: Array[CPIntVar], minVal: Int, lowerLists: Array[Array[(Int, Int)]],
               upperLists: Array[Array[(Int, Int)]])
  extends Constraint(X(0).store) {

  override def associatedVars(): Iterable[CPVar] = X

  override def setup(l: CPPropagStrength): Unit = {
    s.post(new NestedGCCFWC(X, minVal, lowerLists, upperLists))
  }
}

