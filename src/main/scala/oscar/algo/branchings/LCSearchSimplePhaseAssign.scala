/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.algo.branchings

import oscar.algo.reversible._
import oscar.algo.search.{Branching, _}
import oscar.algo.vars.IntVarLike


/**
 * Last Conflict Search with Simple phase saving:
 * at first, branch by assign/remove.
 * Remember the last value tried on every variable in phase.
 * Use phase(i), which is the last value that did not fail on assign,
 * to guide the assignment/split chosen for branching.
 *
 * @author Steven Gay
 * @author Renaud Hartert
 */

class LCSearchSimplePhaseAssign(variables: Array[IntVarLike], varHeuris: Int => Int, valHeuris: Int => Int) extends Branching {

  require(variables.length > 0, "no variables")

  private[this] val nVariables = variables.length
  private[this] val context = variables(0).context

  // Order in which variables have to be assigned
  private[this] val order = Array.tabulate(nVariables) { i => i }

  // Prefered value for each variable
  private[this] val phase = Array.fill(nVariables)(0)

  // Current depth of the search tree
  private[this] val depth = new ReversibleInt(context, 0)

  private[this] var maxDepth: Int = -1

  private[this] var deepest: Int = 0

  final override def reset(): Unit = maxDepth = -1

  final override def alternatives: Seq[Alternative] = {
    val d = updateDepth() // compute the current depth   
    if (d >= nVariables) noAlternative
    else {

      // Trail the new depth
      depth.value = d

      // Use variable heuristic to find next variable to use
      if (d > maxDepth) {
        maxDepth = d
        deepest = findBestHeuristic(d)
        val varId = order(deepest)
        phase(varId) = valHeuris(varId)
      }

      // pull variable that last conflicted or was chosen by variable heuristic up at depth
      if (deepest > d && !variables(order(deepest)).isBound) {
        val deepestInd = order(deepest)
        System.arraycopy(order, d, order, d + 1, deepest - d) // Use a successor vector instead
        order(d) = deepestInd
      }

      deepest = d

      val dd = order(d)
      val x = variables(dd)

      val minX = x.min
      val maxX = x.max
      val p = phase(dd)

      // if phase is in domain, use it to guide search
      if (minX <= p && p <= maxX) {
        // if domain is continuous, do assign/split left/split right
        
        /*
        var s = Seq(() => { store.assign(x, p); if (!store.isFailed) phase(dd) = p })

        if (minX < p) {
          s = s :+ (() => { store.post(x < p); if (!store.isFailed) phase(dd) = p - 1 })
        }

        if (p < maxX) {
          s = s :+ (() => { store.post(x > p); if (!store.isFailed) phase(dd) = p + 1 })
        }

        */
        var s = Seq[() => Unit](() => { context.assign(x, p); if (!context.isFailed) phase(dd) = p })

        if (minX < p) {
          s = s ++ branchOne(context.smaller(x, p))
        }

        if (p < maxX) {
          s = s ++ branchOne(context.larger(x, p))
        }        
        
        s
        // TODO: if domain not continuous, do assign/remove
      } // otherwise, use value heuristic
      else {
        // always do assign/remove
        val v = valHeuris(dd)
        Seq(() => { context.assign(x, v); if (!context.isFailed) phase(dd) = v },
          () => { context.remove(x, v) }
        )
      }
    }
  }

  @inline private def updateDepth(): Int = {
    var d = depth.value
    while (d < nVariables && variables(order(d)).isBound) {
      val varId = order(d)
      phase(varId) = variables(varId).min
      d += 1
    }
    d
  }

  // is this a Scala collection command?
  def findBestHeuristic(depth: Int): Int = {
    var bestIndex = depth
    var bestScore = Int.MaxValue
    var p = depth
    while (p < nVariables) {
      val varId = order(p)
      if (!variables(varId).isBound) {
        val pScore = varHeuris(varId)
        if (pScore < bestScore) {
          bestScore = pScore
          bestIndex = p
        }
      }
      p += 1
    }
    bestIndex
  }
}
