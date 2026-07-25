package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class AtLeastNValue(val x: Array[CPIntVar], val nval: CPIntVar) extends Constraint(x(0).store, "AtLeastNValue") {

  override def associatedVars(): Iterable[CPVar] = x.toSeq :+ nval

  override def setup(l: CPPropagStrength): Unit = {
    if (l == CPPropagStrength.Weak) {
      s.post(new AtLeastNValueFWC(x, nval))
    } else {
      s.post(new AtLeastNValueAC(x, nval))
    }
  }
}
