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
 * Constraint the values minVal+i to appear between low[i] and up[i] times in X
 * @param X The variables to constraint (at least one)
 * @param minVal The smallest value in the interval; its size is determined by the size of lower and upper
 * @param lower The lower bounds for the occurrences of the values of the interval, in order
 * @param upper The upper bounds for the occurrences of the values of the interval, in order
 * @see SoftGCC
 * @see GCCVar
 *
 * @author Victor Lecomte
 */
class GCCFWC(X: Array[CPIntVar], minVal: Int, lower: Array[Int], upper: Array[Int])
  extends Constraint(X(0).store, "GCCFWC") {

  override def associatedVars(): Iterable[CPVar] = X

  idempotent = false

  private[this] val nValues = lower.length
  private[this] val maxVal = minVal + nValues - 1
  private[this] val nBounds = 2 * nValues
  private[this] val nVariables = X.length

  // MEMORIZATION STRUCTURE:
  // The number of variables that are bound to the value
  private[this] var nMandatoryRev: Array[ReversibleInt] = null
  // The number of variables that have the value
  private[this] var nPossibleRev: Array[ReversibleInt] = null
  // The number of bounds that are respected
  private[this] var nBoundsOkRev: ReversibleInt = null
  // The sparse set to memorize the variables having a value that is unbound
  private[this] val unboundSet = Array.ofDim[Int](nValues, nVariables)
  private[this] val unboundIndex = Array.ofDim[Int](nValues, nVariables)

  // Change buffer to load the deltas
  private[this] var changeBuffer: Array[Int] = null

  override def setup(l: CPPropagStrength): Unit = {

    // Temporary variables to avoid using reversible variables too much
    val nMandatory = new Array[Int](nValues)
    val nPossible = new Array[Int](nValues)
    var nBoundsOk = 0

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
        val nBoundsOk = nBoundsOkRev.value
        if (nBoundsOk == nBounds) true
        else whenDomainChanges(delta, x, nBoundsOk)
      })
    }

    // First check loop (according to the counts in the initial counting)
    var vi = nValues
    while (vi > 0) {
      vi -= 1
      val v = vi + minVal
      val mandatory = nMandatory(vi)
      val possible = nPossible(vi)
      val unbound = possible - mandatory
      val thisLower = lower(vi)
      val thisUpper = upper(vi)

      // Few enough variables have the value
      if (possible <= thisUpper) {
        nBoundsOk += 1
      }

      // Too few variables have the value
      if (possible < thisLower) throw Inconsistency
      if (possible == thisLower)
        whenMinPossible(vi, v, unbound)

      // Enough variables are bound to the value
      if (mandatory >= thisLower) {
        nBoundsOk += 1
      }

      // Too many variables are bound to the value
      if (mandatory > thisUpper)
        throw Inconsistency
      if (mandatory == thisUpper)
        whenMaxMandatory(vi, v, unbound)
    }

    // This one has to be initialized in any case
    nBoundsOkRev = new ReversibleInt(s, nBoundsOk)
    
    // If the constraint is not okay yet
    if (nBoundsOk != nBounds) {

      // Initialize the memorization structure
      nMandatoryRev = Array.tabulate(nValues)(vi => new ReversibleInt(s, nMandatory(vi)))
      nPossibleRev = Array.tabulate(nValues)(vi => new ReversibleInt(s, nPossible(vi)))

      // Create the buffer
      changeBuffer = new Array(bufferSize)
    }

    // Everything is up to the closures now
    deactivate()
  }

  /**
   * Update the structure when values are removed from a variable.
   * @return true if the (sub-)constraint has finished its work and should be deactivated
   */
  @inline private def whenDomainChanges(delta: DeltaIntVar, x: CPIntVar, nBoundsOk: Int): Boolean = {
    val i = delta.id
    var nBoundsOkVar = nBoundsOk

    // Treat the value removals
    var c = delta.fillArray(changeBuffer)
    while (c > 0) {
      c -= 1
      val v = changeBuffer(c)
      // If the value removed is one we track
      if (minVal <= v && v <= maxVal) {
        val vi = v - minVal
        // Number of variables having vi in their domain
        val nPossible = nPossibleRev(vi).decr()
        // Number of unbound variables having vi in their domain
        val nUnbound = nPossible - nMandatoryRev(vi).value
        // Variable X(i) must be removed from the unbound variables having vi in their domain since vi was removed
        removeUnbound(i, vi, nUnbound)

        // If the number of variables that have the value decreases to the upper bound, all good!
        if (nPossible == upper(vi)) {
          // the upper-bound for vi cannot be violated
          nBoundsOkVar += 1
          if (nBoundsOkVar == nBounds) {
            nBoundsOkRev.setValue(nBounds)
            return true
          }
        }
        // If the number of variables that have the value decreases to the lower bound, assign the unbound
        if (nPossible == lower(vi))
          whenMinPossible(vi, v, nUnbound)
      }
    }

    // Treat the value assignments
    if (x.isBound) {
      val v = x.min
      // If the value assigned is one we track
      if (minVal <= v && v <= maxVal) {
        val vi = v - minVal
        val nMandatory = nMandatoryRev(vi).incr()
        val nUnbound = nPossibleRev(vi).value - nMandatory
        // Variable X(i) must be removed from the unbound variables having vi in their domain since is it is now bound
        removeUnbound(i, vi, nUnbound)

        // If the number of variables that are bound to the value increases to the lower bound, all good!
        if (nMandatory == lower(vi)) {
          // the lower-bound for vi cannot be violated
          nBoundsOkVar += 1
          if (nBoundsOkVar == nBounds) {
            nBoundsOkRev.setValue(nBounds)
            return true
          }
        }
        // If the number of variables that are bound to the value increases to the upper bound, remove the unbound
        if (nMandatory == upper(vi))
          whenMaxMandatory(vi, v, nUnbound)
      }
    }

    if (nBoundsOkVar != nBoundsOk) {
      nBoundsOkRev.setValue(nBoundsOkVar)
    }
    false
  }

  /**
    * Remove a variable from a value's unbound sparse set
    * i = variable index
    * vi = value removed from X(i)
    * last = current size of the sparse-set
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