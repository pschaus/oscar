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

import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint

/**
 * Circuit constraint
 * Ensures that succ represents a valid circuit. <br>
 * succ[i] represents the city visited after city i. Each city is visited once and
 * there is only one tour.<br>
 * Available propagation strengths are Weak (default) and Strong.
 * @param succ
 * @see CPPropagStrength
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
final class Circuit(succs: Array[CPIntVar], symmetric: Boolean) extends Constraint(succs(0).store, "Circuit") {
  
  require(succs.length > 0, "no variable.")

  override def associatedVars(): Iterable[CPVar] = succs

  private[this] val nSuccs = succs.length
  private[this] val dests = Array.tabulate(nSuccs)(i => new ReversibleInt(s, i))
  private[this] val origs = Array.tabulate(nSuccs)(i => new ReversibleInt(s, i))
  private[this] val lengthToDest = Array.fill(nSuccs)(new ReversibleInt(s,0))
  
  final override def setup(l: CPPropagStrength): Unit = {
    s.post(new AllDifferent(succs:_*), l) // FIXME post two allDifferent in case of symmetry
    var i = nSuccs
    while (i > 0) { i -= 1
      val succ = succs(i)
      succ.removeValue(i)
      if (succ.isBound)
        valBindIdx(succ, i)
      succ.callValBindIdxWhenBind(this, i)
    }

    if (symmetric) {
      val preds = Array.fill(nSuccs)(CPIntVar(0, nSuccs)(s))
      s.post(new Inverse(preds, succs))
      s.post(new Circuit(preds, false))
    }
  }
  
  final override def valBindIdx(x: CPIntVar, i: Int): Unit = {
    val j = x.min
    // o *-> i -> j *-> d
    val d = dests(j).value
    val o = origs(i).value
    // Maintain the path
    dests(o).value = d
    origs(d).value = o
    val length = lengthToDest(o) += (lengthToDest(j).value + 1)
    if (length < nSuccs - 1) succs(d).removeValue(o) // avoid inner loops
  }  
}