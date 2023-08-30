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
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint

/**
  * SubCircuit constraint (only one mode of filtering)
  * This constraint enforces `successors` to represent an Hamiltonian circuit on a subset of nodes.
  * A node that is not part of the circuit is its own successor:
  * succ(i) = j means that j is the successor of i and succ(i) = i means that i is not in the circuit.
  * Available propagation strengths are Weak (default) and Strong.
  * @param succ
  * @see CPPropagStrength
  * @author Pierre Schaus pschaus@gmail.com
  */
final class SubCircuit(succs: Array[CPIntVar]) extends Constraint(succs(0).store, "SubCircuit") {

  require(succs.length > 0, "no variable.")

  override def associatedVars(): Iterable[CPVar] = succs

  private[this] val nSuccs = succs.length
  private[this] val dests = Array.tabulate(nSuccs)(i => new ReversibleInt(s, i))
  private[this] val origs = Array.tabulate(nSuccs)(i => new ReversibleInt(s, i))
  private[this] val lengthToDest = Array.fill(nSuccs)(new ReversibleInt(s,0))
  private[this] val lengthSubCircuit = new ReversibleInt(s,0)

  // two sparse-sets
  // unbound variable indices
  private[this] val unboundIdx = Array.tabulate(nSuccs)(i => i)
  private[this] val nUnbound = new ReversibleInt(s,nSuccs)
  // unbound variable indices such that succs(i) contains i
  private[this] val unboundPossSelfLoopIdx = Array.tabulate(nSuccs)(i => i)
  private[this] val nUnboundPossSelfLoop = new ReversibleInt(s,nSuccs)


  override def setup(l: CPPropagStrength): Unit = {
    if (nSuccs == 1){
      succs(0).assign(0)
      return
    }
    s.post(new AllDifferent(succs), l) // FIXME post two allDifferent in case of symmetry
    for (i <- 0 until nSuccs) {
      if (!succs(i).isBound) {
        if (l == CPPropagStrength.Strong)
          succs(i).callPropagateWhenDomainChanges(this)
        else
          succs(i).callPropagateWhenBind(this)
      }
    }
    propagate()
  }

  override def propagate(): Unit = {

    // filter-out unbound
    var i = nUnboundPossSelfLoop.value
    while (i > 0) {
      i-= 1
      if (!succs(unboundPossSelfLoopIdx(i)).hasValue(unboundPossSelfLoopIdx(i))) {
        // remove this variable that cannot self-loop
        lengthSubCircuit.incr()
        val tmp = unboundPossSelfLoopIdx(i)
        unboundPossSelfLoopIdx(i) = unboundPossSelfLoopIdx(nUnboundPossSelfLoop.value -1)
        unboundPossSelfLoopIdx(nUnboundPossSelfLoop.value -1) = tmp
        nUnboundPossSelfLoop -= 1
      }
      else if (succs(unboundPossSelfLoopIdx(i)).isBound) {
        // remove this bound variable index
        val tmp = unboundPossSelfLoopIdx(i)
        unboundPossSelfLoopIdx(i) = unboundPossSelfLoopIdx(nUnboundPossSelfLoop.value -1)
        unboundPossSelfLoopIdx(nUnboundPossSelfLoop.value -1) = tmp
        nUnboundPossSelfLoop -= 1
      }
    }


    assert(lengthSubCircuit.value != (0 until nSuccs).count(i => !succs(i).hasValue((i))))

    // call bind on every newly bound variable
    i = nUnbound.value
    while (i > 0) {
      i -= 1
      val tmp = unboundIdx(i)
      if (succs(unboundIdx(i)).isBound) {
        bind(tmp)
        unboundIdx(i) =  unboundIdx(nUnbound.value -1)
        unboundIdx(nUnbound.value -1) = tmp
        nUnbound -= 1
      } else {
        val o = origs(tmp).value
        val d = dests(tmp).value
        val lengthOrigDest = lengthToDest(o)
        if (o != tmp && d == tmp) {
          if (lengthSubCircuit.value - 1 > lengthOrigDest) {
            succs(d).removeValue(o)
          }
        }
      }
    }
  }

  private def close(): Unit = {
    var i = nUnboundPossSelfLoop
    while (i > 0) {
      i -= 1
      succs(unboundPossSelfLoopIdx(i)).assign(unboundPossSelfLoopIdx(i))
    }
  }

  private def bind(i: Int): Unit = {
    val j = succs(i).min

    if (j == i) {
      return
    }
    else {
      // o *-> i -> j *-> d
      val d = dests(j).value
      val o = origs(i).value
      val length = lengthToDest(o) += (lengthToDest(j).value + 1)
      if (o == j && i != j) {
        // a sub-circuit was closed but it appears to be too short
        if (length < lengthSubCircuit) {
          throw Inconsistency
        }
        else {
          close()
          return
        }
      }
      else {
        // maintain the path and path length
        dests(o).value = d
        origs(d).value = o
        if (lengthSubCircuit-1 > length) {
          succs(d).removeValue(o)
        }
      }
    }
  }
}

object SubCircuit {
  def apply(successors: Array[CPIntVar], offset: Int = 0): SubCircuit = {
    val succs: Array[CPIntVar] = if (offset == 0) successors else successors.map(_-offset)
    new SubCircuit(succs)
  }
}