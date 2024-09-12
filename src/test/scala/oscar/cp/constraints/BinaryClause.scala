package oscar.cp.constraints

import oscar.cp.core.variables.{CPBoolVar, CPVar}
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength

/** @author Renaud Hartert ren.hartert@gmail.com */
class BinaryClause(x: CPBoolVar, y: CPBoolVar, name: String) extends Constraint(x.store, name) {

  override def associatedVars(): Iterable[CPVar] = Array(x, y)

  final override def setup(l: CPPropagStrength): Unit = {
    propagate()
    x.callPropagateWhenBind(this)
    y.callPropagateWhenBind(this)
  }

  final override def propagate(): Unit = {
    if (x.isTrue)
      deactivate()
    else if (y.isTrue)
      deactivate()
    else if (x.isFalse) {
      y.assign(1)
      deactivate()
    } else if (y.isFalse) { 
      x.assign(1)
      deactivate()
    }
  }
}