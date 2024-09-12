package oscar.cp.constraints.tables

import oscar.cp.core.variables.{CPIntVar, CPVar}
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength

class TableDecomp(val X: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(X(0).store, "TableDecomp"){

  override def associatedVars(): Iterable[CPVar] = X

  override def setup(l: CPPropagStrength): Unit = {
    idempotent = true
    propagate();
    X.filter(!_.isBound).foreach(_.callPropagateWhenDomainChanges(this))
  }

  override def propagate(): Unit = {
    for (i <- 0 until X.size) {
      for (v <- X(i).min to X(i).max if X(i).hasValue(v)) {
        var valueIsSupported = false
        for (tuple <- table if (!valueIsSupported && tuple(i) == v)) {
          var allValueVariableSupported = true
          for (j <- 0 until X.size if (j != i)) {
            if (allValueVariableSupported && !X(j).hasValue(tuple(j)))
              allValueVariableSupported = false
          }
          valueIsSupported = allValueVariableSupported
        }
        if (!valueIsSupported) {
          X(i).removeValue(v)
        }
      }
    }
  }
  
}

    /*
    for (variable <-X; value <- variable) {
      val varIndex = X.indexOf(variable)
      var valueIsSupported = false
      for (tuple <- table if (!valueIsSupported && tuple(varIndex) == value)) {
        var allValueVariableSupported = true
        for (otherVariable <- X if otherVariable != variable) {
          if (allValueVariableSupported && !otherVariable.hasValue(tuple(X.indexOf(otherVariable)))) 
              allValueVariableSupported = false
        }
        valueIsSupported = allValueVariableSupported
      }
      if (!valueIsSupported){
        variable.removeValue(value)
      }
        
    }
    Suspend
    */