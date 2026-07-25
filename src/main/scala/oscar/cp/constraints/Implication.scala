package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class Implication(val A: CPBoolVar, val B: CPBoolVar, val V: CPBoolVar) extends Constraint(A.store, "Implication") {

  override def associatedVars(): Iterable[CPVar] = List(A, B, V)

  override def setup(l: CPPropagStrength): Unit = {
    priorityBindL1 = CPStore.MaxPriorityL1
    if (A.isBound) valBind(A)
    else A.callValBindWhenBind(this)
    
    if (B.isBound) valBind(B)
    else B.callValBindWhenBind(this)
    
    if (V.isBound) valBind(V)
    else V.callValBindWhenBind(this)
  }

  override def valBind(var_ : CPIntVar): Unit = {
    if (A.isBound) {
      if (A.isBoundTo(0)) {
        V.assign(1)
        this.deactivate()
        return
      } else {
        if (B.isBoundTo(0)) {
          V.assign(0)
          this.deactivate()
          return
        }
        if (B.isBoundTo(1)) {
          V.assign(1)
          this.deactivate()
          return
        }
      }
    } 
    if (B.isBound) {
      if (B.isBoundTo(1)) {
        V.assign(1)
        this.deactivate()
        return
      }
    }
    if (V.isBound) {
      if (V.min == 0) {
        A.assign(1)
        B.assign(0)
        this.deactivate()
        return
      } else {
        if (B.isBoundTo(0)) {
          A.assign(0)
          this.deactivate()
          return
        }
        if (B.isBoundTo(1)) {
          this.deactivate()
          return
        }
        if (A.isBoundTo(1)) {
          B.assign(1)
        }
      }
    }
  }
}
