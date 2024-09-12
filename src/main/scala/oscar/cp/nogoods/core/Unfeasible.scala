package oscar.cp.nogoods.core

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPVar

/** @author Renaud Hartert ren.hartert@gmail.com */
class Unfeasible(store: CPStore) extends Constraint(store, "Unfeasible") {
  
  override def setup(l: CPPropagStrength): Unit = throw Inconsistency

  override def associatedVars(): Iterable[CPVar] = Array[CPVar]()
}