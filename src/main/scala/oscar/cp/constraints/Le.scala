package oscar.cp.constraints

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class Le(val x: CPIntVar, val y: CPIntVar) extends Constraint(x.store, " < ") {

  def this(x: CPIntVar, v: Int) = this(x, CPIntVar(x.store, v, v))

  override def associatedVars(): Iterable[CPVar] = List(x, y)

  override def setup(l: CPPropagStrength): Unit = {
    if (y.isBound) {
      x.updateMax(y.min - 1)
      return
    }
    // y > x
    s.post(new Gr(y, x))
  }
}
