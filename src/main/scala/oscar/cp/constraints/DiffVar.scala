package oscar.cp.constraints

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.Constraint

final class DiffVar(x: CPIntVar, y: CPIntVar) extends Constraint(x.store, "DiffVar") {
  
  idempotent = true

  override def associatedVars(): Iterable[CPVar] = Array(x, y)

  final override def setup(l: CPPropagStrength): Unit = {
    init()
    if(isActive) {
      x.callPropagateWhenBind(this)
      y.callPropagateWhenBind(this)
    }
  }
  
  @inline private def init(): Unit = {
    if (x.isBound) {
      y.removeValue(x.min)
      deactivate()
    } else if (y.isBound) {
      x.removeValue(y.min)
      deactivate()
    }
  }

  @inline final override def propagate(): Unit = {
    if (x.isBound)
      y.removeValue(x.min)
    else
      x.removeValue(y.min)
    deactivate()
  }
}