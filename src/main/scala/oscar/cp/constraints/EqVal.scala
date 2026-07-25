package oscar.cp.constraints

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class EqVal(val x: CPIntVar, val v: Int) extends Constraint(x.store, "EqVal") {

  override def associatedVars(): Iterable[CPVar] = List(x)

  override def setup(l: CPPropagStrength): Unit = {
    x.assign(v)
  }
}
