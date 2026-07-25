package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class DiffReifVar(val x: CPIntVar, val y: CPIntVar, val b: CPBoolVar) extends Constraint(x.store, "DiffReifVar") {
  override def associatedVars(): Iterable[CPVar] = List(x, y, b)

  override def setup(l: CPPropagStrength): Unit = {
    if (b.isBound) {
      valBind(b)
    } else if (x.isBound) {
      valBind(x)
    } else if (y.isBound) {
      valBind(y)
    } else {
      x.callPropagateWhenDomainChanges(this)
      y.callPropagateWhenDomainChanges(this)
      b.callValBindWhenBind(this)
      x.callValBindWhenBind(this)
      y.callValBindWhenBind(this)
      propagate()
    }
  }

  override def valBind(var_ : CPIntVar): Unit = {
    if (b.isBound) {
      if (b.min == 1) {
        s.post(new DiffVar(x, y))
      } else {
        s.post(new Eq(x, y))
      }
    } else if (x.isBound) {
      s.post(new DiffReif(y, x.min, b))
    } else if (y.isBound) {
      s.post(new DiffReif(x, y.min, b))
    }
    deactivate()
  }

  override def propagate(): Unit = {
    if (x.getMax < y.getMin) {
      b.assign(1)
      deactivate()
    } else if (y.getMax < x.getMin) {
      b.assign(1)
      deactivate()
    } else {
      val start = Math.max(x.getMin, y.getMin)
      val end = Math.min(x.getMax, y.getMax)
      var commonValues = false
      var i = start
      while (i <= end) {
        if (x.hasValue(i) && y.hasValue(i)) {
          commonValues = true
          i = end + 1 // break
        } else {
          i += 1
        }
      }
      if (!commonValues) {
        b.assign(1)
        deactivate()
      }
    }
  }
}
