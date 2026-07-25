package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPBoolVar
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class BinPacking(val x: Array[CPIntVar], val w: Array[Int], val l: Array[CPIntVar]) extends Constraint(x(0).store, "BinPacking") {

  private var b: Array[Array[CPBoolVar]] = _

  override def associatedVars(): Iterable[CPVar] = x.toSeq ++ l.toSeq

  override def setup(cl: CPPropagStrength): Unit = {
    for (i <- x.indices) {
      x(i).updateMin(0)
      x(i).updateMax(l.length - 1)
    }

    b = Array.ofDim[CPBoolVar](l.length, x.length)
    var totW = 0
    for (j <- x.indices) {
      totW += w(j)
    }
    for (i <- b.indices) {
      for (j <- x.indices) {
        b(i)(j) = x(j).isEq(i)
      }
      s.post(new BinaryKnapsack(b(i), w, l(i)), cl)
    }

    // redundant constraint
    s.post(new Sum(l, CPIntVar(s, totW, totW)))
  }
}
