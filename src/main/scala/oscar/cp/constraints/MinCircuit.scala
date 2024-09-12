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

import oscar.algo.reversible.ReversibleSparseSet
import oscar.cp.core.CPPropagStrength
import oscar.cp._
import oscar.cp.core.variables.CPVar

/**
 * Ensures that succ represents a valid circuit. <br>
 * succ(i) represents the city visited after city i. Each city is visited once and
 * there is only one tour.<br>
 * Available propagation strengths are Weak, Medium and Strong.
 * Weak = elements + circuit + alldiff (AC)
 * Medium = Weak + minAssignment
 * Strong = Medium + Held&Karp Lower-Bounds
 * @param succ
 * @see CPPropagStrength
 * @author Pierre Schaus pschaus@gmail.com
 */
class MinCircuit(val succ: Array[CPIntVar], val distMatrix: Array[Array[Int]], obj: CPIntVar, addPredModel: Boolean = true) extends Constraint(obj.store, "MinCircuit") {

  override def associatedVars(): Iterable[CPVar] = succ ++ Array(obj)

  override def setup(l: CPPropagStrength): Unit = {
    val n = succ.size
    val distMatrixSucc = Array.tabulate(n, n)((i, j) => distMatrix(i)(j))

    val pred = Array.fill(n)(CPIntVar(0 until n)(s))

    s.post(new Circuit(succ, false), Strong)

    s.post(new Sum((0 until n).map(i => distMatrixSucc(i)(succ(i))), obj))

    if (l == CPPropagStrength.Medium || l == CPPropagStrength.Strong) {
      s.post(new MinAssignment(succ, distMatrixSucc, obj))
    }

    if (l == CPPropagStrength.Strong) {
      s.post(new AsymetricHeldKarp(succ, distMatrixSucc, obj))
    }

    if (addPredModel) {
      val distMatrixPred = Array.tabulate(n, n)((i, j) => distMatrixSucc(j)(i))
      val pred = Array.fill(n)(CPIntVar(0 until n)(s))
      s.post(new Inverse(pred, succ), l)
      s.post(new MinCircuit(pred, distMatrixPred, obj, false), l)
    }
  }

}


