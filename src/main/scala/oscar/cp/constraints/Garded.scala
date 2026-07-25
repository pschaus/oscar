package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.variables.CPVar

class Garded(val b: CPBoolVar, val c: Constraint, val onTrue: Boolean) extends Constraint(b.store, "Garded Constraint") {
  override def associatedVars(): Iterable[CPVar] = c.associatedVars().toSeq :+ b

  override def setup(l: CPPropagStrength): Unit = {
    if (!b.isBound) {
      b.callPropagateWhenBind(this)
    } else {
      if ((b.min == 1 && onTrue) || (b.min == 0 && !onTrue)) {
        s.post(c)
      }
      deactivate()
    }
  }

  override def propagate(): Unit = {
    if ((b.min == 1 && onTrue) || (b.min == 0 && !onTrue)) {
      s.post(c)
    }
    deactivate()
  }
}
