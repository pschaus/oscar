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


import oscar.algo.reversible.ReversibleSparseBitSet
import oscar.cp.core.{CPStore, Constraint, _}
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.{CPIntVar, CPIntVarViewOffset, CPVar}

import scala.collection.mutable.ArrayBuffer

/**
 * Implementation of the Compact Table algorithm (CT) for the table constraint.
 * @param X the variables restricted by the constraint.
 * @param table the list of tuples composing the table.
 * @author Pierre Schaus pschaus@gmail.com
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 *
 * Reference(s) :
 *  - Extending Compact-Table to Negative and Short Tables, Helene Verhaeghe, Christophe Lecoutre, Pierre Schaus, AAAI17
 */
final class TableCTNeg(X: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(X(0).store, "TableCTNeg") {

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

  private[this] val dangerousTuples = new ReversibleSparseBitSet(s, nbTuples, 0 until nbTuples)
  private[this] val variableValueAntiSupports = Array.tabulate(arity)(i => new Array[dangerousTuples.BitSet](spans(i)))
  private[this] val deltas: Array[DeltaIntVar] = new Array[DeltaIntVar](arity)

  private[this] val sizeTemp:Array[Int] = Array.tabulate(arity)(i => x(i).size)

  override def setup(l: CPPropagStrength): Unit = {

    /* Success if table is empty initially or after initial filtering */
    if (nbTuples == 0) {
      deactivate()
      return
    }

    /* Retrieve the current dangerous tuples */
    val dangerous = collectDangerousTuples()

    if (dangerous.isEmpty) {
      deactivate()
      return
    }

    /* Remove non dangerous tuples */
    dangerousTuples.collect(new dangerousTuples.BitSet(dangerous))
    dangerousTuples.intersectCollected()

    /* Compute AntiSupports = Compute for each for each variable/value pair the dangerous tuples */
    computeAntiSupports(dangerous)

    /* Call propagate() when domains change */
    var i = 0
    while (i < arity) {
      deltas(i) = x(i).callPropagateOnChangesWithDelta(this)
      i += 1
    }

    /* Propagate a first time */
    basicPropagate()
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

    dangerousTuples.clearCollected()

    /* Update the value of dangerousTuples by considering D(x) or delta */

    if (varSize == 1) {

      /* The variable is assigned */
      dangerousTuples.collect(variableValueAntiSupports(varIndex)(intVar.min))
      dangerousTuples.intersectCollected()

    } else {

      if (delta.size < varSize) {

        /* Use delta to update dangerousTuples */
        domainArraySize = delta.fillArray(domainArray)
        var i = 0
        /* Collect all the removed tuples by doing or's with precomputed masks */
        while (i < domainArraySize) {
          dangerousTuples.collect(variableValueAntiSupports(varIndex)(domainArray(i)))
          i += 1
        }
        /* Remove from the anti-supports all the collected tuples, no longer dangerous */
        dangerousTuples.removeCollected()

      } else {

        /* Don't use delta = reset strategy = recompute from the domain */
        domainArraySize = intVar.fillArray(domainArray)
        var i = 0
        while (i < domainArraySize) {
          dangerousTuples.collect(variableValueAntiSupports(varIndex)(domainArray(i)))
          i += 1
        }
        /* Intersect the set of dangrous tuples with the dangerous tuples collected */
        dangerousTuples.intersectCollected()

      }
    }

    /* Success if there are no more dangerous tuples */
    if (dangerousTuples.isEmpty())
      deactivate()
  }


  /**
   * Perform a consistency check : for each variable value pair (x,a), we check if
   * the number of dangerous tuples doesn't exceed all the possible tuples with the value.
   * Unsupported values are removed.
   * @return the outcome i.e. Failure or Success.
   */
  override def propagate(): Unit = {

    var varIndex = x.length
    while (varIndex > 0) {
      varIndex -= 1
      if (deltas(varIndex).size > 0) {
        updateDelta(varIndex, deltas(varIndex))
        if(!isActive)
          return
      }
    }

    basicPropagate()
  }

  /**
   * Heart of the propagation step
   * Loop on the variable-values until no changes
   * Remove the pair if the number of tuple as reach the threshold
   * @return the outcome i.e. Failure or Suspend
   */
    @inline def basicPropagate(): Unit = {

    var cardinalSizeInit = 1L
    var varIndex = arity
    while (varIndex>0){
      varIndex -= 1
      sizeTemp(varIndex) = x(varIndex).size
      cardinalSizeInit *= sizeTemp(varIndex)
    }

    varIndex = arity
    while (varIndex > 0) {
      varIndex -= 1

      domainArraySize = x(varIndex).fillArray(domainArray)
      var i = domainArraySize
      var value = 0
      val cardinalSize = cardinalSizeInit / sizeTemp(varIndex)

      while (i > 0) {
        i -= 1
        value = domainArray(i)
        val count = dangerousTuples.intersectCount(variableValueAntiSupports(varIndex)(value))
        if (count == cardinalSize) {
          x(varIndex).removeValue(value)
          dangerousTuples.clearCollected()
          dangerousTuples.collect(variableValueAntiSupports(varIndex)(value))
          dangerousTuples.removeCollected()
          if (dangerousTuples.isEmpty()) {
            this.deactivate()
            return
          }
          cardinalSizeInit /= sizeTemp(varIndex)
          sizeTemp(varIndex) -= 1
          cardinalSizeInit *= sizeTemp(varIndex)
        }
      }
    }
  }

  /* ----- Functions used during the setup of the constraint ----- */

  /**
   * Retrieve the dangerous tuples from the table and store their index in dangerousTuplesBuffer.
   * @return the ArrayBuffer containing the dangerous tuples.
   */
  @inline private def collectDangerousTuples(): ArrayBuffer[Int] = {

    val validTuplesBuffer = ArrayBuffer[Int]()

    var tupleIndex = 0
    while (tupleIndex < nbTuples) {
      if (isTupleDangerous(tupleIndex)) {
        validTuplesBuffer += tupleIndex
      }
      tupleIndex += 1
    }

    validTuplesBuffer
  }

  /**
   * Check if a tuple is dangerous.
   * @param tupleIndex the index of the tuple in the table.
   * @return true if the tuple is dangerous, false otherwise.
   */
  @inline private def isTupleDangerous(tupleIndex: Int): Boolean = {
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
  @inline private def computeAntiSupports(dangerous: ArrayBuffer[Int]): Unit = {

    val varValueAntiSupports = Array.tabulate(x.length)(i => Array.tabulate(spans(i))(v => new ArrayBuffer[Int]()))

    /* Collect the supports */
    var dangerousIndex = 0
    while (dangerousIndex < dangerous.length) {
      val tupleIndex = dangerous(dangerousIndex)
      var varIndex = 0
      while (varIndex < arity) {
        val value = T(tupleIndex)(varIndex)
        varValueAntiSupports(varIndex)(value) += tupleIndex
        varIndex += 1
      }
      dangerousIndex += 1
    }

    /* Create the final anti-support bitSets */
    for {
      varIndex <- variableValueAntiSupports.indices
      valueIndex <- variableValueAntiSupports(varIndex).indices
    } {
      variableValueAntiSupports(varIndex)(valueIndex) = new dangerousTuples.BitSet(varValueAntiSupports(varIndex)(valueIndex))
    }
  }
}