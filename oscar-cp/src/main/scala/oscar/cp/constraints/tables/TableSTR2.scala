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
  *******************************************************************************/

package oscar.cp.constraints.tables

import oscar.algo.Inconsistency
import oscar.algo.reversible.{ReversibleBoolean, ReversibleInt, ReversibleSparseSet, SparseSet}
import oscar.cp.core.variables.{CPIntVar, CPIntVarViewOffset, CPVar}
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.CPStore


/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 * @author Sacha Van Cauwelaert sascha.vancauwelaert@gmail.com
 * @author Jordan Demeulenaere
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 * @author Guillaume Perez memocop@gmail.com
 *
 * Implem of: STR2: optimized simple tabular reduction for table constraints, Christophe Lecoutre
 *
 */
final class TableSTR2(private[this] val variables: Array[CPIntVar], private[this] val table: Array[Array[Int]]) extends Constraint(variables(0).store, "TableSTR2") {

  override def associatedVars(): Iterable[CPVar] = variables

  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2 - 1

  private[this] val arity = variables.length

  // Stacks used to represent sSup et SVal
  // sSup is the uninstanciated variables whose domain contains at least one value for which a support has not yet been found
  // sVAL is the uninstanciated variables whose domain has been reduced since the previous invocation of STR2
  private[this] val sSup = new Array[Int](arity)
  private[this] val sVal = new Array[Int](arity)
  private[this] var sSupSize = 0
  private[this] var sValSize = 0

  // Global time-stamp increased at each propagate
  private[this] var timeStamp = 0

  // gacValues and domValues contains at index v-offset(varIdx) the timeStamp at which this value was "set"
  private[this] val gacValues = Array.tabulate(arity)(i => Array.fill(variables(i).max-variables(i).min+1)(-1))
  private[this] val nGacValues = Array.tabulate(arity)(i => 0)
  private[this] val lastGacValue = Array.tabulate(arity)(i => 0)
  private[this] val domValues = Array.tabulate(arity)(i => Array.fill(variables(i).max-variables(i).min+1)(-1))

  private[this] val offsets = Array.tabulate(arity)(i => variables(i).min)
  private[this] val filteredTable = table.filter(t => (0 until arity).forall(i => variables(i).hasValue(t(i))))
  private[this] val T = Array.tabulate(filteredTable.size,arity){case(t,i) => filteredTable(t)(i)-offsets(i)}
  private[this] val x = Array.tabulate(arity)(i => new CPIntVarViewOffset(variables(i),-offsets(i)))

  // Tuples to consider
  private[this] val nTuples = T.length
  private[this] val activeTuples = Array.tabulate(nTuples)(i => i)
  private[this] val nActiveTuplesRev = new ReversibleInt(s, nTuples)
  private[this] var nActiveTuples = 0

  private[this] val sizes = new Array[Int](arity)

  private[this] val unBoundVars = new ReversibleSparseSet(s,0,arity-1)

  private[this] val varIndices = Array.ofDim[Int](arity)
  private[this] val values = Array.ofDim[Int](x.map(_.size).max)

  // Number of variables changed since last propagate
  private[this] var nChanged = 0
  // If number of nChanged = 0, changeIdx is the id of the one that has changed
  private[this] var changedIdx = 0

  // Last size of the domain
  private[this] val lastSize = Array.fill(arity)(new ReversibleInt(s, -1))

  override def setup(l: CPPropagStrength): Unit = {
    propagate()
    if(isActive) {
      var i = arity
      while (i > 0) {
        i -= 1
        if (!x(i).isBound) x(i).callPropagateWhenDomainChanges(this)
      }
    }
  }

  private[this] def validateTuple(tau: Array[Int]): Unit = {
    // Tuple i is thus valid, we need to check every variable
    // for which at least one value has not a support yet (the ones in sSup)
    var j = sSupSize
    while (j > 0) {
      j -= 1
      val varId = sSup(j)
      val value = tau(varId)
      // Value tau(varId) is GAC
      if (gacValues(varId)(value) != timeStamp) {
        gacValues(varId)(value) = timeStamp
        nGacValues(varId) += 1
        lastGacValue(varId) = tau(varId)
        if (nGacValues(varId) == variables(varId).size) {
          // Remove value from sSup
          sSupSize -= 1
          sSup(j) = sSup(sSupSize)
        }
      }
    }
  }

  private def step2() = {
    var i = nActiveTuples
    while (i > 0) {
      i -= 1
      val tau = T(activeTuples(i))
      val isInvalid = isInvalidTuple(tau)
      if (isInvalid) {
        // Deactivate tuple
        nActiveTuples -= 1
        val tmpPosition = activeTuples(i)
        activeTuples(i) = activeTuples(nActiveTuples)
        activeTuples(nActiveTuples) = tmpPosition
      }
      else {
        validateTuple(tau)
      }
    }
  }



  override def propagate(): Unit = {
    // Increasing the timeStamp implicitely removes all dom and gac values
    timeStamp += 1

    // Step1: ----- Initialize and reset GAC values -------

    nChanged = 0
    // Reset SSup and SVal
    sSupSize = 0
    sValSize = 0
    // Cache
    nActiveTuples = nActiveTuplesRev.value

    //var i = arity
    var i = unBoundVars.fillArray(varIndices)
    while (i > 0) {
      i -= 1
      //val varIdx = i
      val varIdx = varIndices(i)
      val varSize = variables(varIdx).size
      val nValues = x(varIdx).fillArray(values)
      nGacValues(varIdx) = 0
      var k = nValues
      while (k > 0) {
        k -= 1
        domValues(varIdx)(values(k)) = timeStamp
      }
      sSup(sSupSize) = varIdx
      sSupSize += 1
      val inSVal = lastSize(varIdx).value != varSize // changed since last propagate
      lastSize(varIdx).setValue(varSize)
      if (inSVal) {
        sVal(sValSize) = varIdx
        sValSize += 1 // push
        changedIdx = varIdx
        nChanged += 1
      }
    }

    // Step2: ----- invalidate tuples and compute GAC values -------

    step2()

    // Not in STR2: no more tuples, so domains will be completely empty anyway
    if (nActiveTuples == 0) {
      throw Inconsistency
    }

    // Step3: ----- Filter the domains -------

    i = sSupSize//.fillArray(varIndices)
    while (i > 0) {
      i -= 1
      val varId = sSup(i) //varIndices(i)
      // Not in STR2: if varId was the only one that has changed, all its values are still consistant
      if (nChanged > 1 || changedIdx != varId) {
        val variable = x(varId)
        val nGac = nGacValues(varId)
        if (nGac == 0) throw Inconsistency
        else if (nGac == 1) {
          variable.assign(lastGacValue(varId))
          unBoundVars.removeValue(varId)
        }
        else {
          var j = variable.fillArray(values)
          while (j > 0) {
            j -= 1
            val v = values(j)
            if (gacValues(varId)(v) != timeStamp) {
              variable.removeValue(v)
            }
          }
          lastSize(varId).setValue(nGac)
        }
      }
    }

    // Trail only if no Failure
    nActiveTuplesRev.value = nActiveTuples
  }


  private def isInvalidTuple(tuple: Array[Int]): Boolean = {
    var i = sValSize
    while (i > 0) {
      i -= 1
      val varId = sVal(i)
      //if (!x(varId).hasValue(tuple(varId))) return true
      if (domValues(varId)(tuple(varId)) != timeStamp) return true
    }
    false
  }

}