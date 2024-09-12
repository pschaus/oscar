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
package oscar.cp.constraints.tables

import oscar.algo.Inconsistency
import oscar.cp.core._
import oscar.algo.reversible._
import oscar.cp._
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.CPVar

/**
 * Implementation of the table algorithm described in :
 *
 * An Optimal Filtering Algorithms for Table Constraints
 * Jean-Baptiste Mairy, Pascal Van Hentenryck and Yves Deville, CP2012
 *
 * @author Pierre Schaus (pschaus@gmail.com)
 */
class TableAC5TCRecomp(val data: TableData, val x: CPIntVar*) extends Constraint(x(0).store, "TableAC5TCRecomp") {

  override def associatedVars(): Iterable[CPVar] = x

  def this(x1: CPIntVar, x2: CPIntVar, tuples: Iterable[(Int, Int)]) = {
    this(new TableData(2), x1, x2)
    tuples.foreach(t => data.add(t._1, t._2))
  }

  def this(x1: CPIntVar, x2: CPIntVar, x3: CPIntVar, tuples: Iterable[(Int, Int, Int)]) = {
    this(new TableData(3), x1, x2, x3)
    tuples.foreach(t => data.add(t._1, t._2, t._3))
  }

  def this(x1: CPIntVar, x2: CPIntVar, x3: CPIntVar, x4: CPIntVar, tuples: Iterable[(Int, Int, Int, Int)]) = {
    this(new TableData(4), x1, x2, x3, x4)
    tuples.foreach(t => data.add(t._1, t._2, t._3, t._4))
  }

  def this(x1: CPIntVar, x2: CPIntVar, x3: CPIntVar, x4: CPIntVar, x5: CPIntVar, tuples: Iterable[(Int, Int, Int, Int, Int)]) = {
    this(new TableData(5), x1, x2, x3, x4, x5)
    tuples.foreach(t => data.add(t._1, t._2, t._3, t._4, t._5))
  }

  assert(data.arity == x.size, { println("TableAC5TCRecomp: mismatch table data arity and x.size") })

  private[this] val domainsFillArray = Array.fill(x.map(_.size).max)(0)

  private[this] val support = Array.fill(x.size)(Array[ReversibleInt]()) // for each variable-value the tuple id that supports it

  private[this] def sup(i: Int)(v: Int) = support(i)(v - data.min(i))

  /**
   * Initialization, input checks and registration to events
   */
  override def setup(l: CPPropagStrength): Unit = {
    idempotent = true
    data.setup()

    for ((y, i) <- x.zipWithIndex) {
      filterAndInitSupport(i)
      if (!y.isBound) {
        y.callOnChangesIdx(i, delta => valuesRemoved(delta), idempotent = true)
      }
    }
  }

  def filterAndInitSupport(i: Int): Unit = {
    x(i).updateMax(data.max(i))
    x(i).updateMin(data.min(i))
    support(i) = Array.fill(data.max(i) - data.min(i) + 1)(new ReversibleInt(s, -1))
    for (v <- x(i).min to x(i).max; if x(i).hasValue(v)) {
      if (data.hasFirstSupport(i, v)) {
        updateAndSeekNextSupport(i, data.firstSupport(i, v), v)
      } else {
        x(i).removeValue(v)
      }
    }
  }

  def updateAndSeekNextSupport(i: Int, startTuple: Int, v: Int): Unit = {
    var t = startTuple
    while (data.hasNextSupport(i, t) && !tupleOk(t)) {
      t = data.nextSupport(i, t)
    }
    if (!tupleOk(t)) {
      x(i).removeValue(v)
    } else {
      sup(i)(v).value = t
    }
  }

  def tupleOk(t: Int): Boolean = {
    // (0 until x.length).forall(i => x(i).hasValue(data(t,i))) // inefficient
    var i = 0
    while (i < x.length) {
      if (!x(i).hasValue(data(t, i))) return false
      i += 1
    }
    return true
  }

  /*
   * x(i) has lost the value tuple(i) so this tuple cannot be a support any more.
   * It means that any pair var-val using tuple as support must find a new support
   */
  def updateSupports(i: Int, t: Int): Unit = {
    var k = 0
    while (k < x.length) {
      //for (k <- 0 until x.size; if (k != i)) {
      if (k != i) {
        val valk = data(t, k) // k_th value in the new invalid tuple t
        if (sup(k)(valk).value == t) { // bad luck, the new invalid tuple was used as support for variable x(k) for value valk
          // so we must find a new one ... or prune the value if none can be found
          updateAndSeekNextSupport(k, t, valk)
        }
      }
      k += 1
    }
  }

  private final def valuesRemoved(delta: DeltaIntVar): Boolean = {
    val idx = delta.id
    var i = delta.fillArray(domainsFillArray)
    while (i > 0) {
      i -= 1
      val value = domainsFillArray(i)
      valueRemoved(x(idx),idx,value)
    }
    false
  }

  private final def valueRemoved(y: CPIntVar, i: Int, v: Int): Unit = {
    // all the supports using a tuple with v at index i are not support any more
    // we iterate on these and try to find new support in case they were used as support
    var t = sup(i)(v).value
    do {
      updateSupports(i, t)
      t = data.nextSupport(i, t) // get the next tuple with a value v at index i
    } while (t >= 0)
  }
}

