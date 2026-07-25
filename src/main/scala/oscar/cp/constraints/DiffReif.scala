package oscar.cp.constraints

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class DiffReif(val x: CPIntVar, val v: Int, val b: CPBoolVar) extends Constraint(x.store, "DiffReif") {
  override def associatedVars(): Iterable[CPVar] = List(x, b)

  override def setup(l: CPPropagStrength): Unit = {
    priorityBindL1 = CPStore.MaxPriorityL1
    priorityRemoveL1 = CPStore.MaxPriorityL1
    
    if (x.isBound || b.isBound) {
      valBind(x)
    } else if (b.isBound) {
      valBind(b)
    } else {
      x.callValBindWhenBind(this)
      b.callValBindWhenBind(this)
      x.callValRemoveWhenValueIsRemoved(this)
    }
  }

  override def updateBounds(var_ : CPIntVar): Unit = {
    if (x.getMax < v || x.getMin > v) {
      b.assign(1)
      deactivate()
    }
  }

  override def valRemove(var_ : CPIntVar, `val`: Int): Unit = {
    if (`val` == v) {
      b.assign(1)
      deactivate()
    }
  }

  override def valBind(var_ : CPIntVar): Unit = {
    if (b.isBound) {
      if (b.min == 1) {
        x.removeValue(v)
      } else {
        x.assign(v)
      }
      deactivate()
    } else if (x.isBound) {
      if (x.min == v) {
        b.assign(0)
      } else {
        b.assign(1)
      }
      deactivate()
    }
  }
}
