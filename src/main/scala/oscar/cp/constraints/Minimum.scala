package oscar.cp.constraints

import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.variables.CPVar

class Minimum(val x: Array[CPIntVar], val y: CPIntVar) extends Constraint(x(0).store, "Minimum") {

  private val maxval = new ReversibleInt(s, 0)
  private val maxvalsupport = new ReversibleInt(s, 0)
  private val minval = new ReversibleInt(s, 0)
  private val minvalsupport = new ReversibleInt(s, 0)

  override def associatedVars(): Iterable[CPVar] = (x.toSeq :+ y)

  private def updateSupport(): Unit = {
    var min = Int.MaxValue
    var max = Int.MaxValue
    for (i <- x.indices) {
      val m = x(i).getMin
      val M = x(i).getMax

      if (m < min) {
        minvalsupport.setValue(i)
        minval.setValue(m)
        min = m
      }
      if (M < max) {
        maxvalsupport.setValue(i)
        maxval.setValue(M)
        max = M
      }
    }
  }

  override def setup(l: CPPropagStrength): Unit = {
    val ymin = y.getMin
    for (i <- x.indices) {
      x(i).updateMin(ymin)
    }
    updateSupport()
    y.updateMin(minval.value)
    y.updateMax(maxval.value)

    for (i <- x.indices) {
      if (!x(i).isBound && (x(i).getMin < y.getMax)) {
        x(i).callUpdateBoundsIdxWhenBoundsChange(this, i)
      }
    }
    if (!y.isBound) {
      y.callUpdateBoundsWhenBoundsChange(this)
    }
  }

  override def updateBoundsIdx(var_ : CPIntVar, idx: Int): Unit = {
    if (idx == minvalsupport.value || idx == maxvalsupport.value) {
      updateSupport()
      y.updateMin(minval.value)
      y.updateMax(maxval.value)
    }

    if (var_.isBound && var_.min == minval.value) {
      y.assign(minval.value)
      deactivate()
    }
  }

  override def updateBounds(var_ : CPIntVar): Unit = {
    val ymin = var_.getMin
    for (i <- x.indices) {
      x(i).updateMin(ymin)
    }
  }
}
