package oscar.cp.constraints

import oscar.algo.Inconsistency
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class SumLeEq(val x: Array[CPIntVar], val y: CPIntVar) extends Constraint(x(0).store, "SumLeq") {

  def this(x: Array[CPIntVar], y: Int) = {
    this(x, CPIntVar(x(0).store, y, y))
  }

  override def associatedVars(): Iterable[CPVar] = x.toSeq :+ y

  override def setup(l: CPPropagStrength): Unit = {
    propagate()
    for (i <- x.indices) {
      if (!x(i).isBound)
        x(i).callPropagateWhenBoundsChange(this)
    }
    if (!y.isBound)
      y.callPropagateWhenBoundsChange(this)
  }

  override def propagate(): Unit = {
    var maxsumx = 0
    var minsumx = 0
    for (i <- x.indices) {
      maxsumx += x(i).getMax
      minsumx += x(i).getMin
    }

    if (maxsumx <= y.getMin) {
      deactivate()
      return
    }

    y.updateMin(minsumx)

    for (i <- x.indices) {
      val minsumxi = minsumx - x(i).getMin
      val maxi = y.getMax - minsumxi
      x(i).updateMax(maxi)
    }
  }
}
