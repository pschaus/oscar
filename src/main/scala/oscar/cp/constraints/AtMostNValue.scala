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

import java.util

import oscar.algo.Inconsistency
import oscar.algo.reversible.{ReversibleInt, ReversibleSparseSet}
import oscar.cp.core.{CPPropagStrength, Constraint}
import oscar.cp.core.variables.{CPIntVar, CPVar}


/**
  * @author Pierre Schaus - pschaus@gmail.com
  * @author Guillaume Derval
  */
class AtMostNValue(val x: Array[CPIntVar], val c: CPIntVar) extends Constraint(x(0).store, "At Most NValue") {
  private[this] val n = x.size

  private[this] val nUnbounds = new ReversibleInt(s,n)
  private[this] val unBounds = Array.tabulate(n)(i => i)

  private[this] val minVal = x.map(_.min).min
  private[this] val maxVal = x.map(_.max).max

  private[this] val nVal = maxVal-minVal +1

  private[this] val unAssignedValues = new ReversibleSparseSet(s,minVal,maxVal)

  private[this] var strength: CPPropagStrength = _

  private[this] val values = Array.ofDim[Int](nVal)

  override def associatedVars(): Iterable[CPVar] = x ++ Array(c)

  override def setup(l: CPPropagStrength): Unit = {
    x.foreach(_.callPropagateWhenBind(this))
    c.callPropagateWhenBoundsChange(this)
    strength = l
    propagate()
  }

  override def propagate(): Unit = {
    var i = nUnbounds.value
    var nU = nUnbounds.value
    while (i > 0) {
      i -= 1
      if (x(unBounds(i)).isBound) {
        nU -= 1
        unAssignedValues.removeValue(x(unBounds(i)).min)
        val tmp = unBounds(i)
        unBounds(i) = unBounds(nU)
        unBounds(nU) = tmp

      }
    }
    nUnbounds.value = nU

    c.updateMin(nVal - unAssignedValues.size)

    if((nVal - unAssignedValues.size) > c.max) {
      throw Inconsistency
    }
    else if ((nVal - unAssignedValues.size) == c.max) {
      var i = unAssignedValues.fillArray(values)
      while (i > 0) {
        i -= 1
        var j = nUnbounds.value
        while (j > 0) {
          j -= 1
          x(unBounds(j)).removeValue(values(i))
        }
      }
      deactivate() //this constraint is satisfied from here
    }
    else if(strength == CPPropagStrength.Strong && (nVal - unAssignedValues.size) == c.max - 1){
      filterUpM1()
    }
  }

  private[this] val assignedValuesCache = Array.ofDim[Int](unAssignedValues.size)
  private[this] val fillArrayCache = Array.ofDim[Int](unAssignedValues.size)

  // helps to compute the intersection in filterUpM1. A value idx is in the intersection if valueCount(idx) == nIntersectableVariable
  private[this] val valueCount = Array.ofDim[Int](unAssignedValues.size)
  private[this] var nIntersectableVariable = 0
  def filterUpM1(): Unit = {
    //The remaining unbound value is in the intersection of the domains of all variables
    //that do not contain an already-used value
    nIntersectableVariable = 0
    util.Arrays.fill(valueCount, 0)

    var nAssignedValues = unAssignedValues.removedFillArray(assignedValuesCache)
    var idx = x.length
    while(idx != 0) {
      idx -= 1
      if(!x(idx).isBound) {
        var ok = true
        var vidx = 0
        while (vidx < nAssignedValues && ok) {
          ok &= !x(idx).hasValue(assignedValuesCache(vidx))
          vidx += 1
        }
        if(ok) {
          var domainSize = x(idx).fillArray(fillArrayCache)
          while(domainSize != 0) {
            domainSize -= 1
            valueCount(fillArrayCache(domainSize)-minVal) += 1
          }
          nIntersectableVariable += 1
        }
      }
    }

    //If we have at least one such set...
    if(nIntersectableVariable >= 1) {

      // Values that are in the already selected values can still be taken, add them to the intersection
      var valIdx = 0
      while (valIdx < nAssignedValues) {
        valueCount(assignedValuesCache(valIdx)-minVal) = nIntersectableVariable
        valIdx += 1
      }

      idx = x.length
      while(idx != 0) {
        idx -= 1
        if(!x(idx).isBound) {
          var domainSize = x(idx).fillArray(fillArrayCache)
          while(domainSize != 0) {
            domainSize -= 1
            if(valueCount(fillArrayCache(domainSize)-minVal) != nIntersectableVariable) {
              x(idx).removeValue(fillArrayCache(domainSize))
            }
          }
        }
      }
    }
  }

}
