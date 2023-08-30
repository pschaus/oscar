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

package oscar.cp.constraints.tables

import oscar.algo.reversible.{ReversibleContext, TrailEntry}
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.{CPPropagStrength, Constraint}

import scala.collection.mutable.ArrayBuffer

/**
 * Implementation of the GAC-4 algorithm for the table constraint.
 * @param X the variables restricted by the constraint.
 * @param initTable the list of tuples composing the table.
 * @author Jordan Demeulenaere j.demeulenaere1@gmail.com
 * @author Pierre Schaus pschaus@gmail.com
 */
class TableGAC4(val X: Array[CPIntVar], initTable: Array[Array[Int]]) extends Constraint(X(0).store, "TableGAC4") {

  override def associatedVars(): Iterable[CPVar] = X

  /* Trailable entry to restore the size of support set of the ith value of a variable */
  final class SupportsTrailEntry(supports: VariableSupports, i: Int, size: Int) extends TrailEntry {
    @inline override def restore(): Unit = supports.restoreSize(i, size)
  }

  /* Reversible Shared Sparse Sets to represent the supports of all values for a single variable */
  class VariableSupports(context: ReversibleContext, nValues: Int, nTuples: Int) {
    private[this] val sparse = Array.fill(nTuples)(-1)
    private[this] val denses = Array.fill(nValues)(null: Array[Int])
    private[this] val sizes = Array.fill(nValues)(0)
    private[this] val lastMagic = Array.fill(nValues)(-1L)
    
    @inline final def apply(index: Int): Array[Int] = denses(index)
    
    @inline final def size(index: Int): Int = sizes(index)
    
    @inline final def clear(index: Int): Unit = {
      trail(index)
      sizes(index) = 0
    }
    
    @inline final def initDense(index: Int, support: Array[Int]): Unit = {
      denses(index) = support
      sizes(index) = support.length
      
      var i = 0
      while (i < support.length) {
        sparse(support(i)) = i
        i += 1
      }
    }
    
    @inline final def remove(index: Int, k: Int): Unit = {
      val dense = denses(index)
      val sK = sparse(k)
      val sL = sizes(index) - 1
      dense(sK) = dense(sL)
      sparse(dense(sK)) = sK
      sparse(k) = sL
      dense(sL) = k
      
      trail(index)
      sizes(index) -= 1
    }
    
    @inline final def trail(index: Int): Unit = {
      val storeMagic = store.magic
      if (lastMagic(index) != storeMagic) {
        lastMagic(index) = storeMagic
        val trailEntry = new SupportsTrailEntry(this, index, sizes(index))
        context.trail(trailEntry)
      }
    }
    
    @inline final def restoreSize(i: Int, newSize: Int): Unit = sizes(i) = newSize
  }
  
  /* Variables arity, minima and spans */
  private[this] val arity = X.length
  private[this] val store = X(0).store
  private[this] val originalMins = Array.tabulate(arity)(i => X(i).min)
  private[this] val spans = Array.tabulate(arity)(i => X(i).max - X(i).min + 1)
  
  /* Reversible supports */
  private[this] val supports = Array.fill(arity)(null: VariableSupports)
  
  /* Allowed tuples */
  private[this] var nbTuples = 0
  private[this] var tuples: Array[Array[Int]] = null
  
  /* Array to fill domains */
  private[this] val domainsFillArray = Array.fill(X.map(_.size).max)(0)
  private[this] var domainSize = 0

  /* ----- Setup ----- */

  override def setup(l: CPPropagStrength): Unit = {
    /* Only work with allowed tuples */
    fillValidTuples()

    /* Fill supports for each (variable, value) pair and remove unsupported values */
    fillSupportsAndRemoveUnsupported()

    X.zipWithIndex.foreach { case (x, index) =>
      x.callOnChangesIdx(index, delta => valuesRemoved(delta),idempotent=true)
    }
  }

  /**
   * Compute the supports for each variable value pair (x,a) and remove values not supported by any tuple.
   * @return the outcome i.e. Failure or Success.
   */
  private final def fillSupportsAndRemoveUnsupported(): Unit = {
    /* Fill temp supports */
    val tempSupports = Array.tabulate(arity)(i => Array.tabulate(spans(i))(j => ArrayBuffer[Int]()))
    var tupleIndex = 0
    var i = -1
    while (tupleIndex < nbTuples) {
      val tuple = tuples(tupleIndex)
      i = 0
      while (i < arity) {
        tempSupports(i)(tuple(i) - originalMins(i)).append(tupleIndex)
        i += 1
      }
      tupleIndex += 1
    }

    /* Set supports in sparse sets */
    i = 0
    while (i < arity) {
      val variableSupport = new VariableSupports(store, spans(i), nbTuples)
      val originalMin = originalMins(i)
      domainSize = X(i).fillArray(domainsFillArray)
      var valueIndex = 0
      while (valueIndex < domainSize) {
        val value = domainsFillArray(valueIndex)
        val tempSupport = tempSupports(i)(value - originalMin)
        if (tempSupport.isEmpty) {
          X(i).removeValue(value)
        }
        else {
          variableSupport.initDense(value - originalMin, tempSupport.toArray)
        }
        
        valueIndex += 1
      }
      supports(i) = variableSupport
      i += 1
    }
  }

  /**
   * Check if a tuple is valid.
   * @param tuple the tuple to check.
   * @return true if the tuple is valid, false otherwise.
   */
  private final def isTupleValid(tuple: Array[Int]): Boolean = {
    var i = 0
    while (i < arity) {
      if (!X(i).hasValue(tuple(i))) {
        return false
      }
      i += 1
    }
    true
  }

  /**
   * Retrieve the valid tuples from the table
   * @return Failure if there is no valid tuples, Suspend otherwise.
   */
  private final def fillValidTuples(): Unit = {
    val tempAllowedTuples = new ArrayBuffer[Array[Int]](initTable.length)
    var tupleIndex = 0
    while (tupleIndex < initTable.length) {
      if (isTupleValid(initTable(tupleIndex))) {
        tempAllowedTuples.append(initTable(tupleIndex))
      }
      tupleIndex += 1
    }

    nbTuples = tempAllowedTuples.size
    tuples = tempAllowedTuples.toArray
  }

  /* ----- Propagation ----- */

  /**
   * Remove a tuple T from supports(x, T[x]).
   * @param tuple the tuple T to remove.
   * @param tupleIndex the index of the tuple.
   * @param varIndex the index of x.
   * @return the outcome i.e. Failure or Success.
   */
  private final def removeTupleFromSupports(tuple: Array[Int], tupleIndex: Int, varIndex: Int): Unit = {
    val valueTuple = tuple(varIndex)
    val valueIndex = valueTuple - originalMins(varIndex)
    val variableSupports = supports(varIndex)
    variableSupports.remove(valueIndex, tupleIndex)
    if (variableSupports.size(valueIndex) == 0)
      X(varIndex).removeValue(tuple(varIndex))
  }

  /**
   * Invalidate a tuple T from all associated supports sets except those associated to the variable at index varIndex.
   * @param tupleIndex the index of the tuple.
   * @param varIndex the index of the variable for which the invalidation is not necessary.
   * @return the outcome i.e. Failure or Success.
   */
  private final def invalidateTuple(tupleIndex: Int, varIndex: Int): Unit = {
    val tuple = tuples(tupleIndex)
    var i = 0
    while (i < varIndex) {
      removeTupleFromSupports(tuple, tupleIndex, i)
      i += 1
    }
    
    i = varIndex + 1
    while (i < arity) {
      removeTupleFromSupports(tuple, tupleIndex, i)
      i += 1
    }
  }

  private final def valuesRemoved(delta: DeltaIntVar): Boolean = {
    val idx = delta.id
    var i = delta.fillArray(domainsFillArray)
    /* Iterate all the values removed in x(idx) */
    while (i > 0) {
      i -= 1
      val value = domainsFillArray(i)
      /* When a value a is removed from D(x), we invalidate all tuples in supports(x,a) */
      val valueIndex = value - originalMins(idx)
      val variableSupports = supports(idx)
      val valueSupports = variableSupports(valueIndex)
      val nbSupports = variableSupports.size(valueIndex)

      var indexSupport = 0
      while (indexSupport < nbSupports) {
        val tupleIndex = valueSupports(indexSupport)
        invalidateTuple(tupleIndex, idx)
        indexSupport += 1
      }
      variableSupports.clear(valueIndex)
    }
    false
  }

}
