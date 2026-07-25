package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class GrEqVarReif(val x: CPIntVar, val y: CPIntVar, val b: CPBoolVar) extends Constraint(x.store, "GrEqVarReif") {
  override def associatedVars(): Iterable[CPVar] = List(x, y, b)

  override def setup(l: CPPropagStrength): Unit = {
    priorityBindL1 = CPStore.MaxPriorityL1 - 1
    if (x.isBound) {
      s.post(new LeEqCteReif(y, x.min, b))
      deactivate()
      return
    } else if (y.isBound) {
      s.post(new GrEqCteReif(x, y.min, b))
      deactivate()
      return
    }
    
    propagate()
    if (isActive) {
      if (!b.isBound) b.callValBindWhenBind(this)
      if (!x.isBound) x.callPropagateWhenBoundsChange(this)
      if (!y.isBound) y.callPropagateWhenBoundsChange(this)
      if (b.isBound) {
        valBind(b)
      }
    }
  }

  override def propagate(): Unit = {
    if (x.getMin >= y.getMax) {
      b.assign(1)
      deactivate()
    } else if (x.getMax < y.getMin) {
      b.assign(0)
      deactivate()
    }
  }

  override def valBind(var_ : CPIntVar): Unit = {
    if (b.min == 0) {
      s.post(new Le(x, y))
    } else {
      s.post(new GrEq(x, y))
    }
    deactivate()
  }
}
