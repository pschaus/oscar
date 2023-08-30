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

package oscar.cp.nogoods.searches

import oscar.cp._
import oscar.algo.reversible.ReversibleInt
import oscar.cp.nogoods.decisions._
import scala.util.Random

/**
 * Abstract Binary Branching:
 * You can specify your variable heuristics
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 * @author Steven Gay steven.gay@uclouvain.be
 *
 * @param varHeuris is a variable heuristic, it will select preferably a random unbound
 *        variables(i) such that varHeuris(i) is the smallest
 */
class RandomHeuristicNogoodBranching(variables: Array[CPIntVar], varHeuris: (Int => Int), valHeuris: (Int => Int), doSplit: Boolean = true)
extends NogoodBranching {

  val cp = variables(0).store

  private[this] val nVariables = variables.length
  private[this] val indexes = Array.tabulate(nVariables)(i => i)
  private[this] val nBounds = new ReversibleInt(cp, 0)

  @inline private def bound(i: Int): Unit = {
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
    var bestH = varHeuris(bestId)
    var sameH = 1 // number of variables with the best heuristic value up to now
    
    i += 1
    while (i < nVariables) {
      val varId = indexes(i)
      val variable = variables(varId)
      if (variable.isBound) bound(i)
      else {
        val h = varHeuris(varId)
        if (h < bestH) {
          bestVariable = variable
          bestId = varId
          bestH = h
          sameH = 1
        }
        else if (h == bestH) {
          sameH += 1
          if (Random.nextInt(sameH) == 0) {
            bestVariable = variable
            bestId = varId
          }
        } 
      }
      i += 1
    }
    bestId
  }

  def nextDecision(): Decision = {
    if (allBounds()) null
    else {
      val i = nextVar()
      val variable = variables(i)
      val value = valHeuris(i)
      if (doSplit) new LowerEq(variable, value) else new Assign(variable, value)      
    }
  }
}
