package oscar.cp.constraints

import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint

class EqualityFC(x: CPIntVar, y: CPIntVar) extends Constraint(x.store, "EqualityFC") {

  override def associatedVars(): Iterable[CPVar] = Array(x, y)

  override def setup(l: CPPropagStrength): Unit = {
    propagate()
    if(isActive) {
      x.callPropagateWhenBind(this)
      y.callPropagateWhenBind(this)
    }
  }

  final override def propagate(): Unit = {
    if (x.isBound)
      y.assign(x.min)
    else
      x.assign(y.min)
    deactivate()
  }
}