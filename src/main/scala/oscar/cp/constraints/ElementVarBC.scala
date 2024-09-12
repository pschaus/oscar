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

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.Constraint
import oscar.cp.util.ArrayUtils
import oscar.algo.reversible.ReversibleInt

import scala.math.min
import scala.math.max
import oscar.cp.core._
import oscar.cp.core.CPSolver
import oscar.cp.core.delta.DeltaIntVar

/**
 * Bound Consistent Element Constraint: y(x) == z
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
final class ElementVarBC(y: Array[CPIntVar], x: CPIntVar, z: CPIntVar) extends Constraint(y(0).store, "ElementVarBC") {

  override def associatedVars(): Iterable[CPVar] = Array(x, z) ++ y

  priorityL2 = CPStore.MaxPriorityL2 - 1
  
  private[this] var supMin: CPIntVar = null
  private[this] var supMax: CPIntVar = null
  private[this] var zMin: Int = 0
  private[this] var zMax: Int = 0

  private[this] val xValues = new Array[Int](x.size)
  
  override def setup(l: CPPropagStrength): Unit = {
    init()
    for (i <- x) y(i).callPropagateWhenBoundsChange(this)
    x.callPropagateWhenDomainChanges(this)
    z.callPropagateWhenBoundsChange(this)
    propagate()
  }

  @inline private def init(): Unit = {
    x.updateMin(0)
    x.updateMax(y.length - 1)
    propagate()
  }

  override def propagate(): Unit = {
    zMin = z.min
    zMax = z.max
    if (x.isBound) equalityPropagate()
    else {
      filterX()
      if (x.isBound)
        equalityPropagate()
      else {
        z.updateMin(supMin.min)
        z.updateMax(supMax.max)
      }
    }
  }

  @inline private def equalityPropagate(): Unit = {
    val id = x.min
    val yVar = y(id)
    yVar.updateMin(zMin)
    yVar.updateMax(zMax)
    z.updateMin(yVar.min)
    z.updateMax(yVar.max)
  }

  @inline private def filterX(): Unit = {
    var min = Int.MaxValue
    var max = Int.MinValue
    var i = x.fillArray(xValues)
    while (i > 0) {
      i -= 1
      val id = xValues(i)
      val yVar = y(id)
      val yMin = yVar.min
      val yMax = yVar.max
      if (yMax < zMin || yMin > zMax) {
        x.removeValue(id)
      } else {
        if (yMin < min) {
          min = yMin
          supMin = yVar
        }
        if (yMax > max) {
          max = yMax
          supMax = yVar
        }
      }
    }
  }
}
