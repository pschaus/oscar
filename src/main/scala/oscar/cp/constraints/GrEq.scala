package oscar.cp.constraints

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class GrEq(val x: CPIntVar, val y: CPIntVar) extends Constraint(x.store, "GrEq") {

  def this(x: CPIntVar, v: Int) = this(x, CPIntVar(x.store, v, v))

  override def associatedVars(): Iterable[CPVar] = List(x, y)

  override def setup(l: CPPropagStrength): Unit = {
    priorityL2 = CPStore.MaxPriorityL2
    propagate()
    if (isActive) {
      if (!y.isBound) y.callPropagateWhenBoundsChange(this)
      if (!x.isBound) x.callPropagateWhenBoundsChange(this)
    }
  }

  override def propagate(): Unit = {
    if (x.getMin >= y.getMax) {
      deactivate()
      return
    }
    x.updateMin(y.getMin)
    y.updateMax(x.getMax)
  }
}
