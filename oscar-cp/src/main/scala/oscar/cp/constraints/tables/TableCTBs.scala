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
import oscar.cp.constraints.tables.tablerepresentation.ShortTable
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.{CPIntVar, CPIntVarViewOffset, CPVar}
import oscar.cp.core.{CPPropagStrength, CPStore, Constraint}

import scala.collection.mutable.ArrayBuffer

object TableCTBs {
  def apply(X: Array[CPIntVar], table: Array[Array[Int]], star:Int = - 1) = {
    val shorttable = new ShortTable(table, star)
    val bstable = shorttable.mapToBasicSmartTable(X).getTable
    new TableCTBs(X, bstable)
  }
}
/**
 * Implementation of the Compact Table algorithm (CT) for the table constraint
 * including tuples with *, !=v, >v, >=v, <v, <=v values, \in S, \not\in S
 * @param X the variables restricted by the constraint.
 * @param table the list of tuples composing the table.
 * @author Pierre Schaus pschaus@gmail.com
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 *
 * Reference(s) :
 *  - Extending Compact-Table to Basic Smart Tables, Helene Verhaeghe, Christophe Lecoutre, Yves Deville, Pierre Schaus, CP17
 */
class TableCTBs(X: Array[CPIntVar], table: Array[Array[BasicSmartElement]]) extends Constraint(X(0).store, "TableCTStarDiffComp") {

  override def associatedVars(): Iterable[CPVar] = X

  /* Setting idempotency & lower priority for propagate() */
  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2 - 1

  /* Basic information */
  private[this] val arity = X.length

  private[this] val spans = Array.tabulate(arity)(i => X(i).max - X(i).min + 1)

  private[this] val offsets = Array.tabulate(arity)(i => X(i).min)
  private[this] val filteredTable = table.filter(t => (0 until arity).forall(i => t(i).isValid(X(i))))

  private[this] val nbTuples = filteredTable.length
  private[this] val x = Array.tabulate(arity)(i => new CPIntVarViewOffset(X(i), -offsets(i)))
  private[this] val T = Array.tabulate(nbTuples, arity) { case (t, i) => filteredTable(t)(i).applyOffset(offsets(i)) }

  private[this] val maxDomain = X.maxBy(_.size).size
  private[this] val domainArray = new Array[Int](maxDomain)
  private[this] var domainArraySize = 0

  private[this] val validTuples = new ReversibleSparseBitSet(s, nbTuples, 0 until nbTuples)
  /* Tuples supporting the value or supporting all value of the variable (aka * fields) */
  private[this] val variableValueSupports = Array.tabulate(arity)(i => Array.fill(spans(i))(new validTuples.BitSet(List())))
  /* Tuples supporting only the value, * field not taken in account as they still stay valid when a value is removed */
  private[this] val variableValueSupportsRM = Array.tabulate(arity)(i => Array.fill(spans(i))(new validTuples.BitSet(List())))
  private[this] val variableValueSupportsMax = Array.tabulate(arity)(i => Array.fill(spans(i))(new validTuples.BitSet(List())))
  private[this] val variableValueSupportsMin = Array.tabulate(arity)(i => Array.fill(spans(i))(new validTuples.BitSet(List())))

  private[this] val deltas: Array[DeltaIntVar] = new Array[DeltaIntVar](arity)

  private[this] val unBoundVars = Array.tabulate(arity)(i => i)
  private[this] val unBoundVarsSize = new ReversibleInt(s, arity)
  private[this] val unBoundVarsSet = Array.tabulate(arity)(i => i)
  private[this] val unBoundVarsSetSize = new ReversibleInt(s, arity)


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

      if (delta.size + 2 < varSize) {
        /* Classical update : use delta to update validTuples */

        domainArraySize = delta.fillArray(domainArray)
        var i = domainArraySize

        if (validTuples.isDuo()) {

          while (i > 0) {
            i -= 1
            validTuples.collect(variableValueSupportsRM(varIndex)(domainArray(i)))
          }
          validTuples.reverseCollected()
          validTuples.collectByIntersection(variableValueSupportsMin(varIndex)(intVar.min))
          validTuples.collectByIntersection(variableValueSupportsMax(varIndex)(intVar.max))

        } else {

          if (delta.minChanged) {
            val _min = intVar.min

            if (delta.maxChanged) {
              val _max = intVar.max

              // Remove support for delta values in [_min;_max]
              while (i > 0) {
                i -= 1
                val value = domainArray(i)
                if (value > _min && value < _max)
                  validTuples.collect(variableValueSupportsRM(varIndex)(value))
              }
              validTuples.reverseCollected()

              // Update from change of max and min value
              validTuples.collectByIntersection(variableValueSupportsMin(varIndex)(_min))
              validTuples.collectByIntersection(variableValueSupportsMax(varIndex)(_max))
            } else {

              // Remove support for delta values in [_min;->
              while (i > 0) {
                i -= 1
                if (domainArray(i) > _min)
                  validTuples.collect(variableValueSupportsRM(varIndex)(domainArray(i)))
              }
              validTuples.reverseCollected()

              // Update from change of min value
              validTuples.collectByIntersection(variableValueSupportsMin(varIndex)(_min))
            }
          } else {

            if (delta.maxChanged) {
              val _max = intVar.max

              // Remove support for delta values in <-;_max]
              while (i > 0) {
                i -= 1
                if (domainArray(i) < _max)
                  validTuples.collect(variableValueSupportsRM(varIndex)(domainArray(i)))
              }
              validTuples.reverseCollected()

              // Update from change of max value
              validTuples.collectByIntersection(variableValueSupportsMax(varIndex)(_max))
            } else {

              // Remove support for delta values
              while (i > 0) {
                i -= 1
                validTuples.collect(variableValueSupportsRM(varIndex)(domainArray(i)))
              }
              validTuples.reverseCollected()
            }
          }
        }
      } else {
        /* Reset update : use domain to update validTuples */
        domainArraySize = intVar.fillArray(domainArray)
        var i = domainArraySize
        while (i > 0) {
          i -= 1
          validTuples.collect(variableValueSupports(varIndex)(domainArray(i)))
        }
      }
    }

    changed = validTuples.intersectCollected()

    /* Failure if there are no more valid tuples */
    if (validTuples.isEmpty())
      throw Inconsistency
  }

  /**
   * Invalidates tuples by handling delta, the set of values removed from D(x) since the last call to this function.
   * @param varIndex the index of x in the array of variables.
   */
  @inline private def updateSet(varIndex: Int): Unit = {
    val intVar = x(varIndex)
    val varSize = intVar.size
    var changed = false

    validTuples.clearCollected()

    /* Update the value of validTuples by considering D(x) or delta */
    if (varSize == 1) {

      /* The variable is assigned */
      validTuples.collect(variableValueSupports(varIndex)(intVar.min))

    } else {

      /* Reset update only: use domain to update validTuples */
      domainArraySize = intVar.fillArray(domainArray)
      var i = domainArraySize
      while (i > 0) {
        i -= 1
        validTuples.collect(variableValueSupports(varIndex)(domainArray(i)))
      }

    }

    /* Intersect the set of valid tuples with the valid tuples collected */
    changed = validTuples.intersectCollected()

    /* Failure if there are no more valid tuples */
    if (validTuples.isEmpty())
      throw Inconsistency
  }

  /**
   * Perform a consistency check : for each variable value pair (x,a), we check if a has at least one valid support.
   * Unsupported values are removed.
   */
  override def propagate(): Unit = {

    var nChanged = -1
    var changedVarIdx = 0

    // Update NonSetVars
    var unBoundVarsSize_ = unBoundVarsSize.value
    var j = unBoundVarsSize_
    while (j > 0) {
      j -= 1
      val varIndex = unBoundVars(j)
      if (deltas(varIndex).size > 0) {
        nChanged += 1
        changedVarIdx = varIndex
        updateDelta(varIndex, deltas(varIndex))
      }
    }

    // Update SetVars
    unBoundVarsSize_ = unBoundVarsSetSize.value
    j = unBoundVarsSize_
    while (j > 0) {
      j -= 1
      val varIndex = unBoundVarsSet(j)
      if (deltas(varIndex).size > 0) {
        nChanged += 1
        changedVarIdx = varIndex
        updateSet(varIndex)
      }
    }

    // Propagate NonSetVars
    propagateUnbound(unBoundVars, unBoundVarsSize, nChanged, changedVarIdx)

    // Propagate SetVars
    propagateUnbound(unBoundVarsSet, unBoundVarsSetSize, nChanged, changedVarIdx)

  }


  @inline private def propagateUnbound(set: Array[Int], size: ReversibleInt, nChanged: Int, changedVarIdx: Int) = {
    var unBoundVarsSize_ = size.value
    var j = unBoundVarsSize_
    while (j > 0) {
      j -= 1
      val varIndex = set(j)

      /* No need to check a variable if it was the only one modified */
      if ((nChanged > 0 || changedVarIdx != varIndex) && !x(varIndex).isBound) {
        domainArraySize = x(varIndex).fillArray(domainArray)
        var i = domainArraySize
        var value = 0
        while (i > 0) {
          i -= 1
          value = domainArray(i)
          if (!validTuples.intersect(variableValueSupports(varIndex)(value))) {
            x(varIndex).removeValue(value)
          }
        }
      }
      if (x(varIndex).isBound) {
        /* If the variable is bound, we never need to consider it any more (put them in a sparse-set) */
        unBoundVarsSize_ -= 1
        set(j) = set(unBoundVarsSize_)
        set(unBoundVarsSize_) = varIndex
      }
    }
    size.value = unBoundVarsSize_
  }

  /* ----- Functions used during the setup of the constraint ----- */

  /**
   * Retrieve the valid tuples from the table and store their index in validTuplesBuffer.
   * @return the ArrayBuffer containing the valid tuples.
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
      if (!T(tupleIndex)(varIndex).isValid(x(varIndex)))
        return false
      varIndex += 1
    }
    true
  }

  /**
   * Compute the mask for each variable value pair (x,a).
   */
  @inline private def computeSupportsAndInitialFiltering(valids: ArrayBuffer[Int]): Unit = {

    // Sort the variable containing sets or not
    var idx = unBoundVarsSize.value
    while (idx > 0) {
      idx -= 1
      var validIndex = valids.length
      var setVar = false
      while (validIndex > 0 && !setVar) {
        validIndex -= 1
        if (T(validIndex)(idx).isInstanceOf[InSet] || T(validIndex)(idx).isInstanceOf[NotInSet])
          setVar = true
      }
      if (setVar){
        val size = unBoundVarsSize.getValue() - 1
        unBoundVars(idx) = unBoundVars(size)
        unBoundVars(size) = idx
        unBoundVarsSize.setValue(size)
      } else {
        val size = unBoundVarsSetSize.getValue() - 1
        unBoundVarsSet(idx) = unBoundVarsSet(size)
        unBoundVarsSet(size) = idx
        unBoundVarsSetSize.setValue(size)
      }
    }

    // Complete support for NonSetVars
    var unBoundSize_ = unBoundVarsSize.value
    idx = unBoundSize_
    while (idx > 0) {
      idx -= 1
      val varIndex = unBoundVars(idx)
      var validIndex = 0
      while (validIndex < valids.length) {
        val tupleIndex = valids(validIndex)
        val smartElem = T(tupleIndex)(varIndex)
        smartElem match {
          case Equal(value) =>
            variableValueSupports(varIndex)(value).set(tupleIndex)
            variableValueSupportsRM(varIndex)(value).set(tupleIndex)
            for (va <- 0 until spans(varIndex)) {
              if (va <= value)
                variableValueSupportsMin(varIndex)(va).set(tupleIndex)
              if (va >= value)
                variableValueSupportsMax(varIndex)(va).set(tupleIndex)
            }
          case NotEqual(value) =>
            for (va <- 0 until spans(varIndex)) {
              if (va != value)
                variableValueSupports(varIndex)(va).set(tupleIndex)
              variableValueSupportsMin(varIndex)(va).set(tupleIndex)
              variableValueSupportsMax(varIndex)(va).set(tupleIndex)
            }
          case Star() =>
            for (va <- 0 until spans(varIndex)) {
              variableValueSupports(varIndex)(va).set(tupleIndex)
              variableValueSupportsMin(varIndex)(va).set(tupleIndex)
              variableValueSupportsMax(varIndex)(va).set(tupleIndex)
            }
          case LessEq(value) =>
            for (va <- 0 until spans(varIndex)) {
              if (va <= value) {
                variableValueSupports(varIndex)(va).set(tupleIndex)
                variableValueSupportsMin(varIndex)(va).set(tupleIndex)
              }
              variableValueSupportsMax(varIndex)(va).set(tupleIndex)
            }
          case GreatEq(value) =>
            for (va <- 0 until spans(varIndex)) {
              if (va >= value) {
                variableValueSupports(varIndex)(va).set(tupleIndex)
                variableValueSupportsMax(varIndex)(va).set(tupleIndex)
              }
              variableValueSupportsMin(varIndex)(va).set(tupleIndex)
            }
        }
        validIndex += 1
      }
    }

    // Complete support for SetVars
    unBoundSize_ = unBoundVarsSetSize.value
    idx = unBoundSize_
    while (idx > 0) {
      idx -= 1
      val varIndex = unBoundVarsSet(idx)
      var validIndex = 0
      while (validIndex < valids.length) {
        val tupleIndex = valids(validIndex)
        val smartElem = T(tupleIndex)(varIndex)
        smartElem match {
          case Equal(value) =>
            variableValueSupports(varIndex)(value).set(tupleIndex)
          case NotEqual(value) =>
            for (va <- 0 until spans(varIndex); if va != value)
              variableValueSupports(varIndex)(va).set(tupleIndex)
          case Star() =>
            for (va <- 0 until spans(varIndex))
              variableValueSupports(varIndex)(va).set(tupleIndex)
          case LessEq(value) =>
            for (va <- 0 to value)
              variableValueSupports(varIndex)(va).set(tupleIndex)
          case GreatEq(value) =>
            for (va <- value until spans(varIndex))
              variableValueSupports(varIndex)(va).set(tupleIndex)
          case InSet(values) =>
            for (va <- 0 until spans(varIndex); if values.contains(va))
              variableValueSupports(varIndex)(va).set(tupleIndex)
          case NotInSet(values) =>
            for (va <- 0 until spans(varIndex); if !values.contains(va))
              variableValueSupports(varIndex)(va).set(tupleIndex)
        }
        validIndex += 1
      }
    }

    // Propagate NonSetVars
    propagateUnbound(unBoundVars, unBoundVarsSize, 1, 0)

    // Propagate SetVars
    propagateUnbound(unBoundVarsSet, unBoundVarsSetSize, 1, 0)
  }
}


