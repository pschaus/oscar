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
import oscar.cp.core.variables.{CPIntVar, CPVar}

/**
 * Alldifferent constraint
 * @author Pierre Schaus pschaus@gmail.com
 * @author Guillaume Derval guillaume.derval@uclouvain.be
 */
class AllDifferentExcept(x: Array[CPIntVar], exclude: Set[Int]) extends Constraint(x(0).store) {

  override def associatedVars(): Iterable[CPVar] = x

  /**
   * Post the constraint that for every pair of variables in x[i], x[j], we have x[i] != x[j] <br>
   * Available propagation strength are Weak (default) and Strong
   * @see CPPropagStrength
   */
  override def setup(l: CPPropagStrength): Unit = {
    val allValues = x.map(_.toSet).foldLeft(Set[Int]())((u,v) => u.union(v))
    if (x.size > allValues.size) throw Inconsistency
    val permutation = allValues.size == x.size

    s.post {
      if (permutation) {
        val minVal = allValues.min
        val maxVal = allValues.max
        val cards = Array.tabulate(maxVal - minVal + 1)(v => {
          if (exclude.contains(v + minVal))
            Int.MaxValue
          else if (allValues.contains(v + minVal))
            1
          else
            0
        })
        new GCCFWC(x, minVal, cards, cards)
      }
      else new AllDiffExceptFWC(x, exclude)
    }

    if(l != CPPropagStrength.Weak) {
      val minVal = x.map(_.min).min
      val maxVal = x.map(_.max).max
      val cards = Array.fill(maxVal - minVal + 1)(1)
      for(i <- exclude)
        cards(i - minVal) = x.length
      s.post(new GCCUpperBC(x, minVal, cards))
    }
  }
}
