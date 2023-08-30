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

package oscar.algo.branchings

import oscar.algo.search._
import oscar.algo.reversible.ReversibleInt
import oscar.algo.search.{Branching, Decision}
import oscar.algo.vars.IntVarLike

/**
 * Abstract Binary Branching:
 * You can specify your variable heuristics
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 *
 * @param varHeuris is a variable heuristic, it will select preferably first the unbound
 *        variables(i) such that varHeuris(i) is the smallest
 */
class BinaryBranching[T](variables: Array[IntVarLike], var varHeuris: Int => T, valHeuris: Int => Int, orderer: T => Ordered[T]) extends Branching {
  private val context = variables(0).context
  private[this] val nVariables = variables.length
  private[this] val indexes = Array.tabulate(nVariables)(i => i)
  private[this] val nBounds = new ReversibleInt(context, 0)

  private def bound(i: Int): Unit = {
    val id = nBounds.incr() - 1
    val tmp = indexes(id)
    indexes(id) = indexes(i)
    indexes(i) = tmp
  }

  protected def allBounds(): Boolean = {
    var i = nBounds.value
    while (i < nVariables) {
      val varId = indexes(i)
      val variable = variables(varId)
      if (variable.isBound) bound(i)
      else return false
      i += 1
    }
    true
  }

  protected def nextVar(): Int = {

    var i = nBounds.value
    var bestId = indexes(i)
    var bestVariable = variables(bestId)
    var bestH: T = varHeuris(bestId)
    i += 1
    while (i < nVariables) {
      val varId = indexes(i)
      val variable = variables(varId)
      if (variable.isBound) bound(i)
      else {
        val h: T = varHeuris(varId)
        val compare = orderer(h).compare(bestH)
        if ((compare < 0) || ((compare == 0) && varId < bestId)) {
          bestVariable = variable
          bestId = varId
          bestH = h
        }
      }
      i += 1
    }
    bestId
  }

  def alternatives(): Seq[Alternative] = {
    if (allBounds()) noAlternative
    else {
      val i = nextVar()
      val variable = variables(i)
      val value = valHeuris(i)
      List(Decision.assign(variable, value), Decision.remove(variable, value))
    }
  }
}
