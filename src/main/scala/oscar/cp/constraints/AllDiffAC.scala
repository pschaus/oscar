package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class AllDiffAC(val x: Array[CPIntVar]) extends Constraint(x(0).store, "Alldifferent AC") {

  override def associatedVars(): Iterable[CPVar] = x.toSeq

  override def setup(l: CPPropagStrength): Unit = {
    val nvalues = CPIntVar(s, x.length)
    s.post(new AtLeastNValueAC(x, nvalues))
  }
}
