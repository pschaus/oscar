package oscar.cp.constraints.tables

import oscar.cp.core.{CPPropagStrength, Constraint}
import oscar.cp.core.variables.{CPIntVar, CPVar}

class TableDecompNeg(val X: Array[CPIntVar], table: Array[Array[Int]]) extends Constraint(X(0).store, "TableDecompNeg"){

  override def associatedVars(): Iterable[CPVar] = X

  override def setup(l: CPPropagStrength): Unit = {
    idempotent = true
    propagate();
    X.filter(!_.isBound).foreach(_.callPropagateWhenDomainChanges(this))
  }

  override def propagate(): Unit = {
    for (i <- X.indices) {
      for (v <- X(i).min to X(i).max if X(i).hasValue(v)) {
        var valueIsSupported = 0
        for (tuple <- table if tuple(i) == v) {
          var allValueVariableSupported = true
          for (j <- X.indices; if (j != i)) {
            if (allValueVariableSupported && !X(j).hasValue(tuple(j)))
              allValueVariableSupported = false
          }
          if (allValueVariableSupported)
            valueIsSupported +=1
        }
        var threashold = 1
        for (j <- X.indices; if (j!=i))
          threashold *= X(j).size
        if (valueIsSupported == threashold) {
          X(i).removeValue(v)
        }
      }
    }
  }
  
}