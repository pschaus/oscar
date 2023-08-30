/** *****************************************************************************
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
  * *****************************************************************************/

package oscar.cp.constraints.tables


import oscar.algo.Inconsistency
import oscar.algo.reversible.{ReversibleInt, ReversibleSparseBitSet}
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.{CPIntVar, CPIntVarViewOffset, CPVar}
import oscar.cp.core.{CPPropagStrength, CPStore, Constraint}

import scala.collection.mutable.ArrayBuffer

/**
 * Implementation of the Compact Table algorithm (CT) for the table constraint.
 * @param X the variables restricted by the constraint.
 * @param table the list of tuples composing the table.
 * @author Pierre Schaus pschaus@gmail.com
 * @author Jordan Demeulenaere j.demeulenaere1@gmail.com
 */
final class TableCT(X: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(X(0).store, "TableCT") {

  override def associatedVars(): Iterable[CPVar] = X

  /* Setting idempotency & lower priority for propagate() */
  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2 - 1

  /* Basic information */
  private[this] val arity = X.length

  private[this] val spans = Array.tabulate(arity)(i => X(i).max - X(i).min + 1)

  private[this] val offsets = Array.tabulate(arity)(i => X(i).min)
  private[this] val filteredTable = table.filter(t => (0 until arity).forall(i => X(i).hasValue(t(i))))

  private[this] val nbTuples = filteredTable.length
  private[this] val T = Array.tabulate(nbTuples, arity) { case (t, i) => filteredTable(t)(i) - offsets(i) }
  private[this] val x = Array.tabulate(arity)(i => new CPIntVarViewOffset(X(i), -offsets(i)))

  private[this] val maxDomain = X.maxBy(_.size).size
  private[this] val domainArray = new Array[Int](maxDomain)
  private[this] var domainArraySize = 0

  private[this] val validTuples = new ReversibleSparseBitSet(s, nbTuples, 0 until nbTuples)
  private[this] val variableValueSupports = Array.tabulate(arity)(i => new Array[validTuples.BitSet](spans(i)))
  private[this] val deltas: Array[DeltaIntVar] = new Array[DeltaIntVar](arity)

  private[this] val unBoundVars = Array.tabulate(arity)(i => i)
  private[this] val unBoundVarsSize = new ReversibleInt(s, arity)

  override def setup(l: CPPropagStrength): Unit = {
    /* Failure if table is empty initially or after initial filtering */
    if (nbTuples == 0)
      throw Inconsistency

    /* Retrieve the current valid tuples */
    val valids = collectValidTuples()

    if (valids.isEmpty)
      throw Inconsistency

    /* Remove non valid tuples */
    validTuples.collect(new validTuples.BitSet(valids))
    validTuples.intersectCollected()

    /* Compute Supports = Compute for each for each variable/value pair the supported tuples,
       Remove values not supported by any tuple */
    computeSupportsAndInitialFiltering(valids)

    /* Call propagate() when domains change */
    var i = 0
    while (i < arity) {
      deltas(i) = x(i).callPropagateOnChangesWithDelta(this)
      i += 1
    }
  }

  private[this] def showTable(): Unit = {
    table.foreach { t =>
      println(t.mkString("\t"))
    }
    println("domains:" + X.mkString(","))
  }

  /**
   * Invalidates tuples by handling delta, the set of values removed from D(x) since the last call to this function.
   * @param varIndex the index of x in the array of variables.
   * @param delta the set of values removed since the last call.
   * @return the outcome i.e. Failure or Success.
   */
  @inline private def updateDelta(varIndex: Int, delta: DeltaIntVar): Unit = {

    val intVar = x(varIndex)
    val varSize = intVar.size
    var changed = false

    validTuples.clearCollected()

    /* Update the value of validTuples by considering D(x) or delta */
    if (varSize == 1) {

      /* The variable is assigned */
      validTuples.collect(variableValueSupports(varIndex)(intVar.min))

    } else {

      if (delta.size < varSize) {

        /* Use delta to update validTuples */
        domainArraySize = delta.fillArray(domainArray)
        var i = 0
        /* Collect all the removed tuples by doing or's with precomputed masks */
        while (i < domainArraySize) {
          validTuples.collect(variableValueSupports(varIndex)(domainArray(i)))
          i += 1
        }

        /* Remove from the valid supports all the collected tuples, no longer supported */
        validTuples.reverseCollected()

      } else {

        /* Don't use delta = reset strategy = recompute from the domain */
        domainArraySize = intVar.fillArray(domainArray)
        var i = 0
        while (i < domainArraySize) {
          validTuples.collect(variableValueSupports(varIndex)(domainArray(i)))
          i += 1
        }

      }

    }
    changed = validTuples.intersectCollected()

    /* Failure if there are no more valid tuples */
    if (validTuples.isEmpty())
      throw Inconsistency
  }


  /**
   * Perform a consistency check : for each variable value pair (x,a), we check if a has at least one valid support.
   * Unsupported values are removed.
   * @return the outcome i.e. Failure or Success.
   */
  override def propagate(): Unit = {

    var nChanged = 0
    var changedVarIdx = 0
    var unBoundVarsSize_ = unBoundVarsSize.value
    var j = unBoundVarsSize.value

    while (j > 0) {
      j -= 1
      val varIndex = unBoundVars(j)

      if (deltas(varIndex).size > 0) {
        nChanged += 1
        changedVarIdx = varIndex
        updateDelta(varIndex, deltas(varIndex))
      }
    }

    /* since we are AC we know some valid tuples have disappeared */
    j = unBoundVarsSize_
    while (j > 0) {
      j -= 1
      val varIndex = unBoundVars(j)

      /* No need to check a variable if it was the only one modified */
      if ((nChanged > 1 || changedVarIdx != varIndex) && !x(varIndex).isBound) {
        domainArraySize = x(varIndex).fillArray(domainArray)
        var i = 0
        var value = 0
        while (i < domainArraySize) {
          value = domainArray(i)
          if (!validTuples.intersect(variableValueSupports(varIndex)(value))) {
            x(varIndex).removeValue(value)
          }
          i += 1
        }
      }
      if (x(varIndex).isBound) {
        /* If the variable is bound, we never need to consider it any more (put them in a sparse-set) */
        unBoundVarsSize_ -= 1
        unBoundVars(j) = unBoundVars(unBoundVarsSize_)
        unBoundVars(unBoundVarsSize_) = varIndex
      }
    }
    unBoundVarsSize.value = unBoundVarsSize_
  }


  /* ----- Functions used during the setup of the constraint ----- */

  /**
   * Retrieve the valid tuples from the table and store their index in validTuplesBuffer.
   * @return Failure if there is no valid tuples, Suspend otherwise.
   */
  @inline private def collectValidTuples(): ArrayBuffer[Int] = {

    val validTuplesBuffer = ArrayBuffer[Int]()

    var tupleIndex = 0
    while (tupleIndex < nbTuples) {
      if (isTupleValid(tupleIndex)) {
        validTuplesBuffer += tupleIndex
      }
      tupleIndex += 1
    }

    validTuplesBuffer
  }

  /**
   * Check if a tuple is valid.
   * @param tupleIndex the index of the tuple in the table.
   * @return true if the tuple is valid, false otherwise.
   */
  @inline private def isTupleValid(tupleIndex: Int): Boolean = {
    var varIndex = 0
    while (varIndex < arity) {
      if (!x(varIndex).hasValue(T(tupleIndex)(varIndex))) {
        return false
      }
      varIndex += 1
    }
    true
  }

  /**
   * Compute the mask for each variable value pair (x,a).
   */
  @inline private def computeSupportsAndInitialFiltering(valids: ArrayBuffer[Int]): Unit = {

    val varValueSupports = Array.tabulate(x.length)(i => Array.tabulate(spans(i))(v => new ArrayBuffer[Int]()))

    /* Collect the supports */
    var validIndex = 0
    while (validIndex < valids.length) {
      val tupleIndex = valids(validIndex)
      var varIndex = 0
      while (varIndex < arity) {
        val value = T(tupleIndex)(varIndex)
        varValueSupports(varIndex)(value) += tupleIndex
        varIndex += 1
      }
      validIndex += 1
    }

    /* Create the final support bitSets and remove any value that is not supported */
    for {
      varIndex <- variableValueSupports.indices
      valueIndex <- variableValueSupports(varIndex).indices
    } {
      if (varValueSupports(varIndex)(valueIndex).nonEmpty) {
        variableValueSupports(varIndex)(valueIndex) = new validTuples.BitSet(varValueSupports(varIndex)(valueIndex))
      } else {
        /* This variable-value does not have any support, it can be removed */
        x(varIndex).removeValue(valueIndex)
      }
    }
  }
}