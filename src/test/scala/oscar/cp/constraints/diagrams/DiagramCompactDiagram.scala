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
 * **************************************************************************** */

package oscar.cp.constraints.diagrams

import oscar.algo.Inconsistency
import oscar.algo.reversible.{ReversibleInt, ReversibleSparseSetSimple}
import oscar.cp.constraints.diagrams.diagramrepresentation.{BasicSmartDiagram, GroundDiagram}
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.variables.{CPIntVar, CPIntVarViewOffset, CPVar}
import oscar.cp.core.{CPPropagStrength, CPStore, Constraint}

/**
 * Implementation of the Compact Diagram algorithm (CD) for the diagram constraint and
 * its extension to the basic smart diagrams constraint
 * (hybrid implementation which adapts depending on the input)
 *
 * @param X       the variables restricted by the constraint.
 * @param diagram the representation of the diagram
 * @author Helene Verhaeghe helene.verhaeghe27@gmail.com
 * @author Pierre Schaus pschaus@gmail.com
 *
 *         Reference(s) :
 *  - Compact-MDD: Efficiently Filtering (s)MDD Constraints with Reversible Sparse Bit-sets, Helene Verhaeghe, Christophe Lecoutre, Pierre Schaus, IJCAI18
 *  - Extending Compact-Diagram to Basic Smart Multi-Valued Variable Diagrams, Helene Verhaeghe, Christophe Lecoutre, Pierre Schaus, CPAIOR19
 */
object DiagramCompactDiagram {

  // Create the propagator Compact-Diagram
  def apply(X: Array[CPIntVar], diagram: GroundDiagram): DiagramCompactDiagram = {
    assert(!X.isEmpty, "Compact-MDD: there should be a non empty array of variables")
    val arity = X.length
    assert(arity == diagram.nbLevel, "Compact-Diagram: number of vars should match Layered Graph size")

    val store = X(0).store

    /* Filter the initial diagram regarding the initial domains */
    val filteredDiagram = diagram.filterDiagram(X).asInstanceOf[GroundDiagram]

    /* Failure if table is empty initially or after initial filtering */
    if (filteredDiagram.isEmpty)
      throw Inconsistency

    /* Offset the domain to obtain domains from 0 */
    val offsets = Array.tabulate(arity)(i => X(i).min)
    val offsetedmdd = filteredDiagram.applyOffset(offsets).asInstanceOf[GroundDiagram]
    val x = Array.tabulate(arity)(i => new CPIntVarViewOffset(X(i), -offsets(i)))

    /* Reversible Sets used to keep track with the remaining nodes of each layer */
    val levels = Array.tabulate(arity + 1)(lvl =>
      new ReversibleSparseSetSimple(store, 0,
        if (lvl != arity)
          offsetedmdd.nbSourceNodes(lvl) - 1
        else
          offsetedmdd.nbDestinationNodes(arity - 1) - 1
      )
    )

    val validEdge = Array.tabulate(arity)(lvl =>
      LayeredReversibleSparseBitSet.apply1(lvl, x(lvl), levels(lvl), levels(lvl + 1), offsetedmdd.getEdges(lvl))
    )
    for (lvl <- 1 until arity)
      validEdge(lvl).putLevelUp(validEdge(lvl - 1))
    for (lvl <- 0 until (arity - 1))
      validEdge(lvl).putLevelDown(validEdge(lvl + 1))

    new DiagramCompactDiagram(store, X, x, arity, validEdge)
  }

  // Create the propagator Compact-Diagram-Bs for Basic smart diagrams
  def apply(X: Array[CPIntVar], diagram: BasicSmartDiagram): DiagramCompactDiagram = {
    assert(!X.isEmpty, "Compact-MDD: there should be a non empty array of variables")
    val arity = X.length
    assert(arity == diagram.nbLevel, "Compact-Diagram: number of vars should match Layered Graph size")

    val store = X(0).store
    val filteredMdd = diagram.filterDiagram(X).asInstanceOf[BasicSmartDiagram]

    /* Failure if table is empty initially or after initial filtering */
    if (filteredMdd.isEmpty)
      throw Inconsistency

    val offsets = Array.tabulate(arity)(i => X(i).min)

    val offsetedmdd = filteredMdd.applyOffset(offsets).asInstanceOf[BasicSmartDiagram]
    val x = Array.tabulate(arity)(i => new CPIntVarViewOffset(X(i), -offsets(i)))

    val levels = Array.tabulate(arity + 1)(lvl =>
      new ReversibleSparseSetSimple(store, 0,
        if (lvl != arity)
          offsetedmdd.nbSourceNodes(lvl) - 1
        else
          offsetedmdd.nbDestinationNodes(arity - 1) - 1
      )
    )

    val validEdge = Array.tabulate(arity)(lvl =>
      LayeredReversibleSparseBitSet.apply2(lvl, x(lvl), levels(lvl), levels(lvl + 1), offsetedmdd.getEdges(lvl))
    )
    for (lvl <- 1 until arity)
      validEdge(lvl).putLevelUp(validEdge(lvl - 1))
    for (lvl <- 0 until (arity - 1))
      validEdge(lvl).putLevelDown(validEdge(lvl + 1))

    new DiagramCompactDiagram(store, X, x, arity, validEdge)
  }
}

class DiagramCompactDiagram private(
                                     private val solver: CPStore,
                                     private val X: Array[CPIntVar],
                                     private val x: Array[CPIntVarViewOffset],
                                     private val arity: Int,
                                     private val validEdge: Array[LayeredReversibleSparseBitSet]
                                   )
  extends Constraint(solver, "Compact-MDD") {

  private[this] val deltas: Array[DeltaIntVar] = new Array[DeltaIntVar](arity)
  private[this] val unBoundVars = Array.tabulate(arity)(i => i)
  private[this] val unBoundVarsSize = new ReversibleInt(s, arity)
  private[this] val lbUnbound = new ReversibleInt(s, arity - 1)
  private[this] val ubUnbound = new ReversibleInt(s, 0)

  /* Setting idempotency & lower priority for propagate() */
  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2 - 1


  override def associatedVars(): Iterable[CPVar] = X

  override def setup(l: CPPropagStrength): Unit = {

    /* Call propagate() when domains change */
    var i = 0
    while (i < arity) {
      deltas(i) = x(i).callPropagateOnChangesWithDelta(this)
      validEdge(i).putDelta(deltas(i))
      i += 1
    }

    initPropagate()

  }

  def initPropagate(): Unit = {
    var nChanged = -1
    var changedVarIdx = 0

    var _lbUnbound = lbUnbound.value
    var _ubUnbound = ubUnbound.value

    validEdge(0).propagateInit(arity-1)

    var unBoundVarsSize_ = unBoundVarsSize.value
    var j = unBoundVarsSize_
    while (j > 0) {
      j -= 1
      val varIndex = unBoundVars(j)

      nChanged += 1
      changedVarIdx = varIndex
      validEdge(varIndex).initUpdate
    }

    validEdge(0).propagateDownSkip(0, arity-1)


    j = unBoundVarsSize_
    while (j > 0) {
      j -= 1
      val varIndex = unBoundVars(j)

      if ((nChanged > 0 || changedVarIdx != varIndex) && !x(varIndex).isBound)
        validEdge(varIndex).propagate

      if (x(varIndex).isBound) {
        /* If the variable is bound, we never need to consider it any more (put them in a sparse-set) */
        unBoundVarsSize_ -= 1
        unBoundVars(j) = unBoundVars(unBoundVarsSize_)
        unBoundVars(unBoundVarsSize_) = varIndex
      } else {
        if (varIndex > _lbUnbound)
          _lbUnbound = varIndex
        if (varIndex < _ubUnbound)
          _ubUnbound = varIndex
      }
    }
    unBoundVarsSize.value = unBoundVarsSize_
    lbUnbound.value = _ubUnbound
    ubUnbound.value = _lbUnbound
  }

  /**
   * Perform a consistency check : for each variable value pair (x,a), we check if a has at least one valid support.
   * Unsupported values are removed.
   */
  override def propagate(): Unit = {
    var nChanged = -1
    var changedVarIdx = 0

    var _lbUnbound = lbUnbound.value
    var _ubUnbound = ubUnbound.value

    validEdge(_lbUnbound).propagateInit(_ubUnbound)

    var unBoundVarsSize_ = unBoundVarsSize.value
    var j = unBoundVarsSize_
    while (j > 0) {
      j -= 1
      val varIndex = unBoundVars(j)

      if (deltas(varIndex).size > 0) {
        nChanged += 1
        changedVarIdx = varIndex
        validEdge(varIndex).update
      }
    }

    validEdge(_lbUnbound).propagateDownSkip(_lbUnbound, _ubUnbound)


    j = unBoundVarsSize_
    while (j > 0) {
      j -= 1
      val varIndex = unBoundVars(j)

      if ((nChanged > 0 || changedVarIdx != varIndex) && !x(varIndex).isBound)
        validEdge(varIndex).propagate

      if (x(varIndex).isBound) {
        /* If the variable is bound, we never need to consider it any more (put them in a sparse-set) */
        unBoundVarsSize_ -= 1
        unBoundVars(j) = unBoundVars(unBoundVarsSize_)
        unBoundVars(unBoundVarsSize_) = varIndex
      } else {
        if (varIndex > _lbUnbound)
          _lbUnbound = varIndex
        if (varIndex < _ubUnbound)
          _ubUnbound = varIndex
      }
    }
    unBoundVarsSize.value = unBoundVarsSize_
    lbUnbound.value = _ubUnbound
    ubUnbound.value = _lbUnbound
  }
}


