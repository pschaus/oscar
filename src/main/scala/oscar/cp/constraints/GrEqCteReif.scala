package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class GrEqCteReif(val x: CPIntVar, val v: Int, val b: CPBoolVar) extends Constraint(x.store, "GrEqCteReif") {
  override def associatedVars(): Iterable[CPVar] = List(x, b)

  override def setup(l: CPPropagStrength): Unit = {
    priorityBindL1 = CPStore.MaxPriorityL1
    priorityL2 = CPStore.MaxPriorityL2 - 1
    propagate()
    if (isActive) {
      b.callValBindWhenBind(this)
      x.callPropagateWhenBoundsChange(this)
      if (b.isBound) {
        valBind(b)
      }
    }
  }

  override def propagate(): Unit = {
    if (x.getMin >= v) {
      b.assign(1)
      deactivate()
    } else if (x.getMax < v) {
      b.assign(0)
      deactivate()
    }
  }

  override def valBind(var_ : CPIntVar): Unit = {
    if (b.min == 0) {
      x.updateMax(v - 1)
    } else {
      x.updateMin(v)
    }
    deactivate()
  }
}
