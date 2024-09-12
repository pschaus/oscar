package oscar.cp.constraints

import oscar.cp.core.variables.{CPBoolVar, CPIntVar, CPVar}
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore

/** @author Renaud Hartert ren.hartert@gmail.com */
final class EqReifBC(int: CPIntVar, value: Int, boolean: CPBoolVar) extends Constraint(int.store, "EqReif") {

  override def associatedVars(): Iterable[CPVar] = Array(int, boolean)

  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2

  final override def setup(l: CPPropagStrength): Unit = {
    propagate()
    if(isActive) {
      int.callPropagateWhenBoundsChange(this)
      boolean.callPropagateWhenBind(this)
    }
  }

  final override def propagate(): Unit = {
    if (boolean.isFalse) {
      if (int.min == value) {
        int.updateMin(value + 1)
        deactivate()
      } else if (int.max == value) {
        int.updateMax(value - 1)
        deactivate()
      }
    }
    else if (boolean.isTrue) {
      int.assign(value)
      deactivate()
    }
    else if (!int.hasValue(value)) {
      boolean.assignFalse()
      deactivate()
    } else if (int.isBound) {
      boolean.assignTrue()
      deactivate()
    }
  }
}