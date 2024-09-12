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
package oscar.cp.constraints

import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.Constraint
import oscar.cp.core.CPStore

/**
 * Implementation of Sum Constraint:
 * @author Pierre Schaus pschaus@gmail.com
 * @author Steven Gay steven.gay@uclouvain.be
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class Sum(x: Array[CPIntVar], constant: Int, sum: CPIntVar) extends Constraint(sum.store, "Sum") {

  override def associatedVars(): Iterable[CPVar] = x ++ Array(sum)

  // Alternative constructor
  def this(x: Array[CPIntVar], sum: CPIntVar) = this(x, 0, sum)

  // Internal structure
  private[this] val nVariables = x.length
  private[this] val variables = Array.tabulate(nVariables)(i => x(i).asInstanceOf[CPIntVar])
  private[this] val fixedValue = new ReversibleInt(s, constant)
  private[this] val nFixed = new ReversibleInt(s, 0)

  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2 - 1

  final override def setup(l: CPPropagStrength): Unit = {
    propagate()
    if(isActive) {
      sum.callPropagateWhenBoundsChange(this)
      var i = 0
      while (i < nVariables) {
        variables(i).callPropagateWhenBoundsChange(this)
        i += 1
      }
    }
  }

  // Invariant: fixedValue is the sum of bound term variables.
  override def propagate(): Unit = {

    var sumxmin = 0
    var sumxmax = 0
    var maxDiff = 0
    var reduce = true
    var n = nFixed.value
    var value = fixedValue.value

    while (reduce) {
      
      reduce = false
      sumxmin = value
      sumxmax = value

      // step 1: filter bound variables and get maximum range size 
      var i = n
      while (i < nVariables) {
        val variable = variables(i)
        val min = variable.min
        val max = variable.max
        sumxmin += min
        sumxmax += max
        val diff = max - min
        // Update fixed variables
        if (diff == 0) {
          value += min
          val tmp = variables(n)
          variables(n) = variable
          variables(i) = tmp
          n += 1
        }
        else if (maxDiff < diff) maxDiff = diff
        i += 1
      }

      // step 2: propagate from x to y
      sum.updateMax(sumxmax)
      sum.updateMin(sumxmin)
      
      val ymax = sum.max
      val ymin = sum.min

      // step 3: propagate from y to x: if there is a ximax that makes the sum go over y, find and trim it
      if (sumxmin + maxDiff > ymax) {
        i = n
        while (i < nVariables) {
          // Compute new upper bound for xi
          val oldximax = variables(i).max
          val ymini = sumxmin - variables(i).min
          val ximax = ymax - ymini
          // Update and check whether the domain is sparse
          if (ximax < oldximax) {
            variables(i).updateMax(ximax)
            val newximax = variables(i).max
            val xidiff = newximax - oldximax
            sumxmax += xidiff
            reduce |= newximax != ximax // this can happen when x(i) is a multiplicative or non-bijective view ; then do fixpoint here.
          }
          i += 1
        }
      }

      // step 4: same for ximin
      if (sumxmax - maxDiff < ymin) {
        i = n
        while (i < nVariables) {
          val oldximin = variables(i).min
          val ymaxi = sumxmax - variables(i).max
          val ximin = ymin - ymaxi
          // Update and check whether the domain is sparse
          if (ximin > oldximin) {
            variables(i).updateMin(ximin)
            val newxmin = variables(i).min
            reduce |= newxmin != ximin
          }
          i += 1
        }
      }

    }
    
    nFixed.value = n
    fixedValue.value = value
  }
}
