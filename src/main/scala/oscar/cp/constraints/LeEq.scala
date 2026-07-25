package oscar.cp.constraints

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class LeEq(val x: CPIntVar, val y: CPIntVar) extends Constraint(x.store, "LeEq") {

  def this(x: CPIntVar, v: Int) = this(x, CPIntVar(x.store, v, v))

  override def associatedVars(): Iterable[CPVar] = List(x, y)

  override def setup(l: CPPropagStrength): Unit = {
    s.post(new GrEq(y, x))
  }
}
