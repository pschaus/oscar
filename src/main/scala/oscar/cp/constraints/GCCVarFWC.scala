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
import oscar.algo.reversible._
import oscar.cp.core._
import oscar.cp.core.delta._
import oscar.cp.core.variables._

/**
 * Global Cardinality Constraint
 *
 * Constraint the values minVal+i to appear between boundingVar(i) times
 * @param X The variables to constraint (at least one)
 * @param minVal The smallest value in the interval; its size is determined by the size of lower and upper
 * @param boundingVar The variables that bound the occurrences
 * @see SoftGCC
 * @see GCCVar
 *
 * @author Victor Lecomte
 */
class GCCVarFWC(X: Array[CPIntVar], minVal: Int, boundingVar: Array[CPIntVar])
  extends Constraint(X(0).store, "GCCVarFWC") {

  override def associatedVars(): Iterable[CPVar] = X ++ boundingVar

  idempotent = false

  private[this] val nValues = boundingVar.length
  private[this] val maxVal = minVal + nValues - 1
  private[this] val nVariables = X.length

  // MEMORIZATION STRUCTURE:
  // The number of variables that are bound to the value
  private[this] var nMandatoryRev: Array[ReversibleInt] = null
  // The number of variables that have the value
  private[this] var nPossibleRev: Array[ReversibleInt] = null
  private[this] var valueOkRev: Array[ReversibleBoolean] = null
  // The number of bounds that are respected
  private[this] var nValuesOkRev: ReversibleInt = null
  // The sparse set to memorize the variables having a value that is unbound
  private[this] val unboundSet = Array.tabulate(nValues)(vi => new Array[Int](nVariables))
  private[this] val unboundIndex = Array.tabulate(nValues)(vi => new Array[Int](nVariables))

  // Change buffer to load the deltas
  private[this] var changeBuffer: Array[Int] = null

  override def setup(l: CPPropagStrength): Unit = {

    // Temporary variables to avoid using reversible variables too much
    val nMandatory = new Array[Int](nValues)
    val nPossible = new Array[Int](nValues)
    val valueOk = new Array[Boolean](nValues)
    var nValuesOk = 0

    // Initial counting (the rest is done in the update functions)
    var bufferSize = 0
    var i = nVariables
    while (i > 0) {
      i -= 1
      val x = X(i)

      // Count the number of variables that are bound to the value
      if (x.isBound) {
        val v = x.min
        if (minVal <= v && v <= maxVal) {
          val vi = v - minVal
          nMandatory(vi) += 1
        }
      }
      // Count the number of variables that have the value
      var vi = nValues
      while (vi > 0) {
        vi -= 1
        if (x.hasValue(vi + minVal)) {
          nPossible(vi) += 1

          // If x is unbound, add it to the value's sparse set
          if (!x.isBound) {
            val index = nPossible(vi) - nMandatory(vi) - 1
            unboundSet(vi)(index) = i
            unboundIndex(vi)(i) = index
          }
        }
      }

      // Adapt the change buffer size
      if (bufferSize < x.size) {
        bufferSize = x.size
      }

      // Register before the first check loop so that we receive information on what we changed there
      x.callOnChangesIdx(i, delta => {
        val nValuesOk = nValuesOkRev.value
        if (nValuesOk == nValues) true
        else whenDomainChanges(delta, x, nValuesOk)
      })
    }

    // Register for changes in the bounding variables (also important in case there are common elements between `X` and
    //  `boundingVar`)
    var vi = nValues
    while (vi > 0) {
      vi -= 1
      // Do array indexing in advance
      val bound = boundingVar(vi)
      // Since using a variable in the closure is probably not a good idea
      val vi_fixed = vi
      bound.filterWhenBoundsChange()({
        if (valueOkRev(vi_fixed).value) true
        else whenBoundsChange(bound, vi_fixed)
      })
    }

    // INITIAL CHECK LOOP
    vi = nValues
    while (vi > 0) {
      vi -= 1

      val v = vi + minVal
      val bound = boundingVar(vi)
      var lower = bound.min
      var upper = bound.max
      val possible = nPossible(vi)
      val mandatory = nMandatory(vi)
      val unbound = possible - mandatory

      // Restrict the bounding to what is possible
      if (upper > possible) {
        bound.updateMax(possible)
        upper = possible
      }
      if (lower < mandatory) {
        bound.updateMin(mandatory)
        lower = mandatory
      }

      // Too few variables have the value
      if (possible < lower) {
        throw Inconsistency
      }
      // Just enough variables have the value
      else if (possible == lower) {

        // Try to bind them all
        whenMinPossible(vi, v, unbound)

        // The correct number of variables have the value, and we have bound them all
        if (lower == upper) {
          nValuesOk += 1
          valueOk(vi) = true
        }
      }

      if (!valueOk(vi)) {
        // Too many variables are bound to the value
        if (mandatory > upper) {
          throw Inconsistency
        }
        // Just few enough variables are bound to the value
        else if (mandatory == upper) {

          // Try to remove the unbound
          whenMaxMandatory(vi, v, unbound)

          // The correct number of variables are bound to the value, and we have removed the unbound
          if (lower == upper) {
            nValuesOk += 1
            valueOk(vi) = true
          }
        }
      }
    }
    // END OF INITIAL CHECK LOOP

    // Initialize the memorization structure
    nMandatoryRev = Array.tabulate(nValues)(vi => new ReversibleInt(s, nMandatory(vi)))
    nPossibleRev = Array.tabulate(nValues)(vi => new ReversibleInt(s, nPossible(vi)))
    valueOkRev = Array.tabulate(nValues)(vi => new ReversibleBoolean(s, valueOk(vi)))
    nValuesOkRev = new ReversibleInt(s, nValuesOk)

    // Create the buffer
    changeBuffer = new Array(bufferSize)

    // Everything is up to the closures now
    this.deactivate()
  }

  /**
   * Update the structure when values are removed from a variable.
   */
  @inline private def whenDomainChanges(delta: DeltaIntVar, x: CPIntVar, nValuesOk: Int): Boolean = {
    val i = delta.id
    var nValuesOkVar = nValuesOk

    // Treat the value removals
    var c = delta.fillArray(changeBuffer)
    while (c > 0) {
      c -= 1
      val v = changeBuffer(c)

      // If the value removed is one we track
      if (minVal <= v && v <= maxVal) {
        //if (v == -2) println("Removing -2 from " + i)
        val vi = v - minVal
        if (!valueOkRev(vi).value) {

          val possible = nPossibleRev(vi).decr()
          val unbound = possible - nMandatoryRev(vi).value

          removeUnbound(i, vi, unbound)

          // Restrict the bounding to what is possible
          val bound = boundingVar(vi)
          if (bound.max > possible) {
            bound.updateMax(possible)
            // Tricky case when `bound` is the same variable as `x` and we still want to process the value removal.
            // See "Magic Sequence" example.
            if (bound.==(x)) {
              changeBuffer(c) = possible + 1
              c += 1
            }
          }

          // Just enough variables have the value
          val lower = bound.min
          if (possible == lower) {

            // Try to bind them all
            whenMinPossible(vi, v, unbound)

            // The correct number of variables have the value, and we have bound them all
            valueOkRev(vi).setTrue()
            nValuesOkVar += 1
            if (nValuesOkVar == nValues) {
              nValuesOkRev.setValue(nValues)
              return true
            }
          }
        }
      }
    }

    // Treat the value assignments
    if (x.isBound) {
      val v = x.min

      // If the value assigned is one we track
      if (minVal <= v && v <= maxVal) {
        val vi = v - minVal
        if (!valueOkRev(vi).value) {

          val mandatory = nMandatoryRev(vi).incr()
          val unbound = nPossibleRev(vi).value - mandatory

          removeUnbound(i, vi, unbound)

          // Restrict the bounding to what is possible
          val bound = boundingVar(vi)
          bound.updateMin(mandatory)

          // Just few enough variables are bound to the value
          val upper = bound.max
          if (mandatory == upper) {

            // Try to remove the unbound
            whenMaxMandatory(vi, v, unbound)

            // The correct number of variables are bound to the value, and we have removed the unbound
            valueOkRev(vi).setTrue()
            nValuesOkVar += 1
            if (nValuesOkVar == nValues) {
              nValuesOkRev.setValue(nValues)
              return true
            }
          }
        }
      }
    }

    if (nValuesOkVar != nValuesOk) {
      nValuesOkRev.setValue(nValuesOkVar)
    }
    false
  }

  def whenBoundsChange(bound: CPIntVar, vi: Int): Boolean = {
    val v = vi + minVal
    val lower = bound.min
    val upper = bound.max
    val possible = nPossibleRev(vi).value
    val mandatory = nMandatoryRev(vi).value
    val unbound = possible - mandatory

    // Just enough variables have the value
    if (possible == lower) {

      // Try to bind them all
      whenMinPossible(vi, v, unbound)

      // The correct number of variables have the value, and we have bound them all
      nValuesOkRev.incr()
      valueOkRev(vi).setTrue()
      return true
    }

    // Just few enough variables are bound to the value
    if (mandatory == upper) {

      // Try to remove the unbound
      whenMaxMandatory(vi, v, unbound)

      // The correct number of variables are bound to the value, and we have removed the unbound
      nValuesOkRev.incr()
      valueOkRev(vi).setTrue()
      return true
    }
    false
  }

  /**
   * Remove a variable from a value's unbound sparse set
   */
  @inline private def removeUnbound(i: Int, vi: Int, last: Int): Unit = {

    val thisUnboundSet = unboundSet(vi)
    val thisUnboundIndex = unboundIndex(vi)
    val atLast = thisUnboundSet(last)
    val index = thisUnboundIndex(i)

    thisUnboundSet(index) = atLast
    thisUnboundIndex(atLast) = index
    thisUnboundSet(last) = i
    thisUnboundIndex(i) = last
  }

  /**
   * When the number of possible variables drops to the lower bound, bind the unbound.
   */
  @inline private def whenMinPossible(vi: Int, v: Int, nUnbound: Int): Unit = {
    val thisUnboundSet = unboundSet(vi)

    // Bind all the unbound variables that have this value
    var i = nUnbound
    while (i > 0) {
      i -= 1
      X(thisUnboundSet(i)).assign(v)
    }
  }

  /**
   * When the number of mandatory variables reaches the upper bound, drop the unbound.
   */
  @inline private def whenMaxMandatory(vi: Int, v: Int, nUnbound: Int): Unit = {
    val thisUnboundSet = unboundSet(vi)

    // Remove the value from the unbound variables that have this value
    var i = nUnbound
    while (i > 0) {
      i -= 1
      X(thisUnboundSet(i)).removeValue(v)
    }
  }
}